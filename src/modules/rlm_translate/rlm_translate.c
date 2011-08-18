/*
 * rlm_translate.c
 *
 *   This program is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 51 Franklin St, Fifth Floor,
 *   Boston, MA 02110-1301, USA
 *
 */

/*
 *   This is a rewrite of the gnu 'tr' command, modified for freeradius
 *   by Oliver Schroeder <oliver.schroeder@versatel.de>
 *
 *   The original code is
 *   Copyright (C) 1991, 1995-2010 Free Software Foundation, Inc.
 *   Written by Jim Meyering
 */

#include <freeradius-devel/ident.h>
#include <freeradius-devel/radiusd.h>
#include <freeradius-devel/modules.h>
#include <freeradius-devel/rad_assert.h>

#ifdef HAVE_REGEX_H
#	include <regex.h>
#endif
#include <limits.h>	/* FIXME: check via autoconf */
#include <ctype.h>	/* FIXME: check via autoconf */
#include "xstrtol.h"

#define RLM_REGEX_INPACKET 0
#define RLM_REGEX_INCONFIG 1
#define RLM_REGEX_INREPLY  2
#define RLM_REGEX_INPROXY 3
#define RLM_REGEX_INPROXYREPLY 4
#define ISDIGIT(c) ((unsigned int) (c) - '0' <= 9)

/* The value for Spec_list->state that indicates to
   get_next that it should initialize the tail pointer.
   Its value should be as large as possible to avoid conflict
   a valid value for the state field -- and that may be as
   large as any valid repeat_count.  */
#define BEGIN_STATE (UINTMAX_MAX - 1)

/* The value for Spec_list->state that indicates to
   get_next that the element pointed to by Spec_list->tail is
   being considered for the first time on this pass through the
   list -- it indicates that get_next should make any necessary
   initializations.  */
#define NEW_ELEMENT (BEGIN_STATE + 1)

/* The maximum possible repeat count.  Due to how the states are
   implemented, it can be as much as BEGIN_STATE.  */
#define REPEAT_COUNT_MAXIMUM BEGIN_STATE
#define ARRAY_CARDINALITY(Array) (sizeof (Array) / sizeof *(Array))

enum { N_CHARS = UCHAR_MAX + 1 };

/* An unsigned integer type big enough to hold a repeat count or an
   unsigned character.  POSIX requires support for repeat counts as
   high as 2**31 - 1.  Since repeat counts might need to expand to
   match the length of an argument string, we need at least size_t to
   avoid arbitrary internal limits.  It doesn't cost much to use
   uintmax_t, though.  */
typedef uintmax_t count;

/* The following (but not CC_NO_CLASS) are indices into the array of
   valid character class strings.  */
enum Char_class
{
	CC_ALNUM = 0, CC_ALPHA = 1, CC_BLANK = 2, CC_CNTRL = 3,
	CC_DIGIT = 4, CC_GRAPH = 5, CC_LOWER = 6, CC_PRINT = 7,
	CC_PUNCT = 8, CC_SPACE = 9, CC_UPPER = 10, CC_XDIGIT = 11,
	CC_NO_CLASS = 9999
};

static char const *const char_class_name[] =
{
	"alnum", "alpha", "blank", "cntrl", "digit", "graph",
	"lower", "print", "punct", "space", "upper", "xdigit"
};

/* Character class to which a character (returned by get_next) belonged;
   but it is set only if the construct from which the character was obtained
   was one of the character classes [:upper:] or [:lower:].  The value
   is used only when translating and then, only to make sure that upper
   and lower class constructs have the same relative positions in string1
   and string2.  */
enum Upper_Lower_class
{
	UL_LOWER,
	UL_UPPER,
	UL_NONE
};

/* The type of a List_element.  See build_spec_list for more details.  */
enum Range_element_type
{
	RE_NORMAL_CHAR,
	RE_RANGE,
	RE_CHAR_CLASS,
	RE_EQUIV_CLASS,
	RE_REPEATED_CHAR
};

/* One construct in one of tr's argument strings.
   For example, consider the POSIX version of the classic tr command:
       tr -cs 'a-zA-Z_' '[\n*]'
   String1 has 3 constructs, two of which are ranges (a-z and A-Z),
   and a single normal character, `_'.  String2 has one construct.  */
struct List_element
{
	enum Range_element_type type;
	struct List_element *next;
	union
	{
		unsigned char normal_char;
		struct                  /* unnamed */
		{
			unsigned char first_char;
			unsigned char last_char;
		} range;
		enum Char_class char_class;
		unsigned char equiv_code;
		struct                  /* unnamed */
		{
			unsigned char the_repeated_char;
			count repeat_count;
		} repeated_char;
	} u;
};

/* Each of tr's argument strings is parsed into a form that is easier
   to work with: a linked list of constructs (struct List_element).
   Each Spec_list structure also encapsulates various attributes of
   the corresponding argument string.  The attributes are used mainly
   to verify that the strings are valid in the context of any options
   specified (like -s, -d, or -c).  The main exception is the member
   `tail', which is first used to construct the list.  After construction,
   it is used by get_next to save its state when traversing the list.
   The member `state' serves a similar function.  */
struct Spec_list
{
	/* Points to the head of the list of range elements.
	   The first struct is a dummy; its members are never used.  */
	struct List_element *head;

	/* When appending, points to the last element.  When traversing via
	   get_next(), points to the element to process next.  Setting
	   Spec_list.state to the value BEGIN_STATE before calling get_next
	   signals get_next to initialize tail to point to head->next.  */
	struct List_element *tail;

	/* Used to save state between calls to get_next.  */
	count state;

	/* Length, in the sense that length ('a-z[:digit:]123abc')
	   is 42 ( = 26 + 10 + 6).  */
	count length;

	/* The number of [c*] and [c*0] constructs that appear in this spec.  */
	size_t n_indefinite_repeats;

	/* If n_indefinite_repeats is nonzero, this points to the List_element
	   corresponding to the last [c*] or [c*0] construct encountered in
	   this spec.  Otherwise it is undefined.  */
	struct List_element *indefinite_repeat_element;

	/* True if this spec contains at least one equivalence
	   class construct e.g. [=c=].  */
	int has_equiv_class;

	/* True if this spec contains at least one character class
	   construct.  E.g. [:digit:].  */
	int has_char_class;

	/* True if this spec contains at least one of the character class
	   constructs (all but upper and lower) that aren't allowed in s2.  */
	int has_restricted_char_class;
};

/* A representation for escaped string1 or string2.  As a string is parsed,
   any backslash-escaped characters (other than octal or \a, \b, \f, \n,
   etc.) are marked as such in this structure by setting the corresponding
   entry in the ESCAPED vector.  */
struct E_string
{
	char *s;
	int *escaped;
	size_t len;
};

typedef struct rlm_translate_t {
	/* The attribute to search for */
	char* attribute;

	/* The attribute number */
	int   attr_num;

	/* The pattern to search for */
	char* set1;
	/* The length of the search pattern */
	int   set1_len;

	/* The VALUE_PAIR list to search in.
	   Can be either packet,reply,proxy,
	   proxy_reply or control (plus it's
	   alias 'config') */
	char* searchin_str;

	/* The same as above just coded as a number
	   for speed */
	char  searchin;

	/* The replacement */
	char* set2;

	/* The length of the replacement string */
	int   set2_len;

	/* use the complement of search */
	int   complement;

	/* delete characters in search, do not
	   translate */
	int   delete;

	/* replace each input sequence of a repeated
	   character that is listed in search with
	   a single occurrence of that character */
	int   squeeze;

	/* translate characters in set1 to
	   characters in set2 */
	int   translate;

	/* Array of boolean values.  A character `c' is a member of the
	   squeeze set if and only if in_squeeze_set[c] is true.  The squeeze
	   set is defined by the last (possibly, the only) string argument
	   on the command line when the squeeze option is given.  */
	int   in_squeeze_set[N_CHARS];

	/* Array of boolean values.  A character `c' is a member of the
	   delete set if and only if in_delete_set[c] is true.  The delete
	   set is defined by the first (or only) string argument on the
	   command line when the delete option is given.  */
	int   in_delete_set[N_CHARS];

	/* Array of character values defining the translation (if any) that
	   tr is to perform.  Translation is performed only when there are
	   two specification strings and the delete switch is not given.  */
	int   xlate[N_CHARS];
	struct Spec_list *s1;
	struct Spec_list *s2;

	/* The module name */
	const char *name;
} rlm_translate_t;

static const CONF_PARSER module_config[] = {
  { "attribute",	PW_TYPE_STRING_PTR,	offsetof(rlm_translate_t,attribute), NULL, NULL },
  { "set1",		PW_TYPE_STRING_PTR,	offsetof(rlm_translate_t,set1), NULL, "" },
  { "searchin",		PW_TYPE_STRING_PTR,	offsetof(rlm_translate_t,searchin_str), NULL, "packet" },
  { "set2",		PW_TYPE_STRING_PTR,	offsetof(rlm_translate_t,set2), NULL, "" },
  { "complement",	PW_TYPE_BOOLEAN,	offsetof(rlm_translate_t,complement), NULL, "no" },
  { "delete",		PW_TYPE_BOOLEAN,	offsetof(rlm_translate_t,delete), NULL, "no" },
  { "squeeze",		PW_TYPE_BOOLEAN,	offsetof(rlm_translate_t,squeeze), NULL, "no" },
  { "translate",	PW_TYPE_BOOLEAN,	offsetof(rlm_translate_t,translate), NULL, "no" },
  { NULL, -1, 0, NULL, NULL }
};

/* C89 compliant way to cast 'char' to 'unsigned char'. */
static inline unsigned char
to_uchar (char ch)
{
	return ch;
}

static void
spec_init (struct Spec_list *spec_list)
{
	struct List_element *new = rad_malloc (sizeof *new);
	spec_list->head = spec_list->tail = new;
	spec_list->head->next = NULL;
}

static void
es_free (struct E_string *es)
{
	free (es->s);
	free (es->escaped);
}

/* Perform the first pass over each range-spec argument S, converting all
   \c and \ddd escapes to their one-byte representations.  If an invalid
   quote sequence is found print an error message and return false;
   Otherwise set *ES to the resulting string and return true.
   The resulting array of characters may contain zero-bytes;
   however, on input, S is assumed to be null-terminated, and hence
   cannot contain actual (non-escaped) zero bytes.  */
static int
unquote (char const *s, struct E_string *es)
{
	size_t	i, j;
	size_t	len = strlen (s);

	es->s = rad_malloc (len);
	es->escaped = calloc (len, sizeof es->escaped[0]);
	j = 0;
	for (i = 0; s[i]; i++) {
		unsigned char c;
		int oct_digit;
		switch (s[i])
		{
		case '\\':
			es->escaped[j] = 1;
			switch (s[i + 1])
			{
			case '\\':
				c = '\\';
				break;
			case 'a':
				c = '\a';
				break;
			case 'b':
				c = '\b';
				break;
			case 'f':
				c = '\f';
				break;
			case 'n':
				c = '\n';
				break;
			case 'r':
				c = '\r';
				break;
			case 't':
				c = '\t';
				break;
			case 'v':
				c = '\v';
				break;
			case '0':
			case '1':
			case '2':
			case '3':
			case '4':
			case '5':
			case '6':
			case '7':
				c = s[i + 1] - '0';
				oct_digit = s[i + 2] - '0';
				if (0 <= oct_digit && oct_digit <= 7) {
					c = 8 * c + oct_digit;
					++i;
					oct_digit = s[i + 2] - '0';
					if (0 <= oct_digit && oct_digit <= 7) {
						if (8 * c + oct_digit < N_CHARS) {
							c = 8 * c + oct_digit;
							++i;
						} else {
							/* A 3-digit octal number larger than \377 won't
							fit in 8 bits.  So we stop when adding the
							next digit would put us over the limit and
							give a warning about the ambiguity.  POSIX
							isn't clear on this, and we interpret this
							lack of clarity as meaning the resulting behavior
							is undefined, which means we're allowed to issue
							a warning.  */
							radlog(L_ERR, "rlm_translate: warning: the ambiguous octal escape "
								"\\%c%c%c is being\n\tinterpreted as the 2-byte sequence \\0%c%c, %c",
								s[i], s[i + 1], s[i + 2],
								s[i], s[i + 1], s[i + 2]);
						}
					}
				}
				break;
			case '\0':
				radlog(L_ERR, "rlm_translate: warning: warning: an unescaped backslash at end of string is not portable");
				/* POSIX is not clear about this.  */
				es->escaped[j] = 0;
				i--;
				c = '\\';
				break;
			default:
				c = s[i + 1];
				break;
			}
		++i;
		es->s[j++] = c;
		break;
		default:
		es->s[j++] = s[i];
		break;
		}
	}
	es->len = j;
	return 1;
}

/* Return nonzero if the Ith character of escaped string ES matches C
   and is not escaped itself.  */
static inline int
es_match (struct E_string const *es, size_t i, char c)
{
	return ( (es->s[i] == c) && (!es->escaped[i]) );
}

/* Search forward starting at START_IDX for the 2-char sequence
   (PRE_BRACKET_CHAR,']') in the string P of length P_LEN.  If such
   a sequence is found, set *RESULT_IDX to the index of the first
   character and return true.  Otherwise return false.  P may contain
   zero bytes.  */
static int
find_closing_delim (const struct E_string *es, size_t start_idx,
                    char pre_bracket_char, size_t *result_idx)
{
	size_t i;
	for (i = start_idx; i < es->len - 1; i++) {
		if ( (es->s[i] == pre_bracket_char) && (es->s[i + 1] == ']')
		  && (! es->escaped[i]) && (! es->escaped[i + 1])) {
			*result_idx = i;
			return 1;
		}
	}
	return 0;
}

/* If CLASS_STR is a valid character class string, return its index
   in the global char_class_name array.  Otherwise, return CC_NO_CLASS.  */
static enum Char_class
look_up_char_class (char const *class_str, size_t len)
{
	enum Char_class i;
	for (i = 0; i < ARRAY_CARDINALITY (char_class_name); i++) {
		if ( (strncmp (class_str, char_class_name[i], len) == 0)
		&&   (strlen (char_class_name[i]) == len) )
			return i;
	}
	return CC_NO_CLASS;
}

/* Return true if the string at ES->s[IDX] matches the regular
   expression `\*[0-9]*\]', false otherwise.  The string does not
   match if any of its characters are escaped.  */
static int
star_digits_closebracket (const struct E_string *es, size_t idx)
{
	size_t i;
	if (!es_match (es, idx, '*'))
		return 0;
	for (i = idx + 1; i < es->len; i++) {
		if (!ISDIGIT (to_uchar (es->s[i])) || es->escaped[i])
			return es_match (es, i, ']');
	}
	return 0;
}

/* If CHAR_CLASS_STR is a valid character class string, append a
   newly allocated structure representing that character class to the end
   of the specification list LIST and return true.  If CHAR_CLASS_STR is not
   a valid string return false.  */
static int
append_char_class (struct Spec_list *list,
                   char const *char_class_str, size_t len)
{
	enum	Char_class char_class;
	struct	List_element *new;

	char_class = look_up_char_class (char_class_str, len);
	if (char_class == CC_NO_CLASS)
		return 0;
	new = rad_malloc (sizeof *new);
	new->next = NULL;
	new->type = RE_CHAR_CLASS;
	new->u.char_class = char_class;
	rad_assert (list->tail);
	list->tail->next = new;
	list->tail = new;
	return 1;
}

/* Return a newly allocated copy of S which is suitable for printing.
   LEN is the number of characters in S.  Most non-printing
   (isprint) characters are represented by a backslash followed by
   3 octal digits.  However, the characters represented by \c escapes
   where c is one of [abfnrtv] are represented by their 2-character \c
   sequences.  This function is used solely for printing error messages.  */
static char *
make_printable_str (char const *s, size_t len)
{
	/* Worst case is that every character expands to a backslash
	followed by a 3-character octal escape sequence.  */
	char *printable_buf = rad_malloc ((len + 1) * 4);
	char *p = printable_buf;
	size_t i;

	for (i = 0; i < len; i++) {
		char buf[5];
		char const *tmp = NULL;
		unsigned char c = s[i];

		switch (c)
		{
		case '\\':
			tmp = "\\";
			break;
		case '\a':
			tmp = "\\a";
			break;
		case '\b':
			tmp = "\\b";
			break;
		case '\f':
			tmp = "\\f";
			break;
		case '\n':
			tmp = "\\n";
			break;
		case '\r':
			tmp = "\\r";
			break;
		case '\t':
			tmp = "\\t";
			break;
		case '\v':
			tmp = "\\v";
			break;
		default:
			if (isprint (c)) {
				buf[0] = c;
				buf[1] = '\0';
			} else {
				sprintf (buf, "\\%03o", c);
			}
			tmp = buf;
			break;
		}
		p = stpcpy (p, tmp);
	}
	return printable_buf;
}

/* Given a string, EQUIV_CLASS_STR, from a [=str=] context and
   the length of that string, LEN, if LEN is exactly one, append
   a newly allocated structure representing the specified
   equivalence class to the specification list, LIST and return true.
   If LEN is not 1, return false.  */
static int
append_equiv_class (struct Spec_list *list,
                    char const *equiv_class_str, size_t len)
{
	struct List_element *new;

	if (len != 1)
		return 0;
	new = rad_malloc (sizeof *new);
	new->next = NULL;
	new->type = RE_EQUIV_CLASS;
	new->u.equiv_code = *equiv_class_str;
	rad_assert (list->tail);
	list->tail->next = new;
	list->tail = new;
	return 1;
}

/* Parse the bracketed repeat-char syntax.  If the P_LEN characters
   beginning with P[ START_IDX ] comprise a valid [c*n] construct,
   then set *CHAR_TO_REPEAT, *REPEAT_COUNT, and *CLOSING_BRACKET_IDX
   and return zero. If the second character following
   the opening bracket is not `*' or if no closing bracket can be
   found, return -1.  If a closing bracket is found and the
   second char is `*', but the string between the `*' and `]' isn't
   empty, an octal number, or a decimal number, print an error message
   and return -2.  */
static int
find_bracketed_repeat (const struct E_string *es, size_t start_idx,
                       unsigned char *char_to_repeat, count *repeat_count,
                       size_t *closing_bracket_idx)
{
	size_t i;

	rad_assert (start_idx + 1 < es->len);
	if (!es_match (es, start_idx + 1, '*'))
		return -1;
	for (i = start_idx + 2; (i < es->len) && (! es->escaped[i]); i++) {
		if (es->s[i] == ']') {
			size_t digit_str_len = i - start_idx - 2;
			*char_to_repeat = es->s[start_idx];
			if (digit_str_len == 0) {
				/* We've matched [c*] -- no explicit repeat count.  */
				*repeat_count = 0;
			} else {
				/* Here, we have found [c*s] where s should be a string
				of octal (if it starts with `0') or decimal digits.  */
				char const *digit_str = &es->s[start_idx + 2];
				char *d_end;
				if ((xstrtoumax (digit_str, &d_end, *digit_str == '0' ? 8 : 10, repeat_count, NULL) != LONGINT_OK)
				|| REPEAT_COUNT_MAXIMUM < *repeat_count
				|| digit_str + digit_str_len != d_end) {
					char *tmp = make_printable_str (digit_str, digit_str_len);
					radlog(L_ERR, "rlm_translate: invalid repeat count %s in [c*n] construct", tmp);
					free (tmp);
					return -2;
				}
			}
			*closing_bracket_idx = i;
			return 0;
		}
	}
	return -1; /* No bracket found.  */
}

/* Return a newly allocated string with a printable version of C.
   This function is used solely for formatting error messages.  */
static char *
make_printable_char (unsigned char c)
{
	char *buf = rad_malloc (5);

	if (isprint (c)) {
		buf[0] = c;
		buf[1] = '\0';
	} else {
		sprintf (buf, "\\%03o", c);
	}
	return buf;
}

/* Append a newly allocated structure representing a [c*n]
   repeated character construct to the specification list LIST.
   THE_CHAR is the single character to be repeated, and REPEAT_COUNT
   is a non-negative repeat count.  */
static void
append_repeated_char (struct Spec_list *list, unsigned char the_char,
                      count repeat_count)
{
	struct List_element *new;

	new = rad_malloc (sizeof *new);
	new->next = NULL;
	new->type = RE_REPEATED_CHAR;
	new->u.repeated_char.the_repeated_char = the_char;
	new->u.repeated_char.repeat_count = repeat_count;
	rad_assert (list->tail);
	list->tail->next = new;
	list->tail = new;
}

/* Append a newly allocated structure representing the range
   of characters from FIRST to LAST to the specification list LIST.
   Return false if LAST precedes FIRST in the collating sequence,
   true otherwise.  This means that '[c-c]' is acceptable.  */
static int
append_range (struct Spec_list *list, unsigned char first, unsigned char last)
{
	struct List_element *new;

	if (last < first) {
		char *tmp1 = make_printable_char (first);
		char *tmp2 = make_printable_char (last);

		radlog(L_ERR, "rlm_translate: range-endpoints of `%s-%s' are in reverse collating sequence order", tmp1, tmp2);
		free (tmp1);
		free (tmp2);
		return 0;
	}
	new = rad_malloc (sizeof *new);
	new->next = NULL;
	new->type = RE_RANGE;
	new->u.range.first_char = first;
	new->u.range.last_char = last;
	rad_assert (list->tail);
	list->tail->next = new;
	list->tail = new;
	return 1;
}

/* Append a newly allocated structure representing a
   character C to the specification list LIST.  */
static void
append_normal_char (struct Spec_list *list, unsigned char c)
{
	struct List_element *new;

	new = rad_malloc (sizeof *new);
	new->next = NULL;
	new->type = RE_NORMAL_CHAR;
	new->u.normal_char = c;
	rad_assert (list->tail);
	list->tail->next = new;
	list->tail = new;
}

/* Convert string UNESCAPED_STRING (which has been preprocessed to
   convert backslash-escape sequences) of length LEN characters into
   a linked list of the following 5 types of constructs:
      - [:str:] Character class where `str' is one of the 12 valid strings.
      - [=c=] Equivalence class where `c' is any single character.
      - [c*n] Repeat the single character `c' `n' times. n may be omitted.
          However, if `n' is present, it must be a non-negative octal or
          decimal integer.
      - r-s Range of characters from `r' to `s'.  The second endpoint must
          not precede the first in the current collating sequence.
      - c Any other character is interpreted as itself.  */
static int
build_spec_list (const struct E_string *es, struct Spec_list *result)
{
	char const *p;
	size_t i;

	p = es->s;
	/* The main for-loop below recognizes the 4 multi-character constructs.
	   A character that matches (in its context) none of the multi-character
	   constructs is classified as `normal'.  Since all multi-character
	   constructs have at least 3 characters, any strings of length 2 or
	   less are composed solely of normal characters.  Hence, the index of
	   the outer for-loop runs only as far as LEN-2.  */
	for (i = 0; i + 2 < es->len; /* empty */) {
		if (es_match (es, i, '[')) {
			int matched_multi_char_construct;
			size_t closing_bracket_idx;
			unsigned char char_to_repeat;
			count repeat_count;
			int err;
			matched_multi_char_construct = 1;
			if (es_match (es, i + 1, ':') || es_match (es, i + 1, '=')) {
				size_t closing_delim_idx;
				if (find_closing_delim (es, i + 2, p[i + 1], &closing_delim_idx)) {
					size_t opnd_str_len = closing_delim_idx - 1 - (i + 2) + 1;
					char const *opnd_str = p + i + 2;
					if (opnd_str_len == 0) {
						if (p[i + 1] == ':')
							radlog(L_ERR, "rlm_translate: missing character class name '[::]'");
						else
							radlog(L_ERR, "rlm_translate: missing missing equivalence class character '[==]'");
						return 0;
					}
					if (p[i + 1] == ':') {
						/* FIXME: big comment.  */
						if (!append_char_class (result, opnd_str, opnd_str_len)) {
							if (star_digits_closebracket (es, i + 2))
								goto try_bracketed_repeat;
							else {
								char *tmp = make_printable_str (opnd_str, opnd_str_len);
								radlog(L_ERR, "rlm_translate: invalid character class %s", tmp);
								free (tmp);
								return 0;
							}
						}
					} else {
						/* FIXME: big comment.  */
						if (!append_equiv_class (result, opnd_str, opnd_str_len)) {
							if (star_digits_closebracket (es, i + 2))
								goto try_bracketed_repeat;
							else {
								char *tmp = make_printable_str (opnd_str, opnd_str_len);
								radlog(L_ERR, "rlm_translate: %s: equivalence class operand must be a single character", tmp);
								free (tmp);
								return 0;
							}
						}
					}
					i = closing_delim_idx + 2;
					continue;
				}
				/* Else fall through.  This could be [:*] or [=*].  */
			}
try_bracketed_repeat:
			/* Determine whether this is a bracketed repeat range
			matching the RE \[.\*(dec_or_oct_number)?\].  */
			err = find_bracketed_repeat (es, i + 1, &char_to_repeat, &repeat_count, &closing_bracket_idx);
			if (err == 0) {
				append_repeated_char (result, char_to_repeat, repeat_count);
				i = closing_bracket_idx + 1;
			} else if (err == -1) {
				matched_multi_char_construct = 0;
			} else {
				/* Found a string that looked like [c*n] but the
				numeric part was invalid.  */
				return 0;
			}
			if (matched_multi_char_construct)
				continue;
			/* We reach this point if P does not match [:str:], [=c=],
			[c*n], or [c*].  Now, see if P looks like a range `[-c'
			(from `[' to `c').  */
		}
		/* Look ahead one char for ranges like a-z.  */
		if (es_match (es, i + 1, '-')) {
			if (!append_range (result, p[i], p[i + 2]))
				return 0;
			i += 3;
		} else {
			append_normal_char (result, p[i]);
			++i;
		}
	}
	/* Now handle the (2 or fewer) remaining characters p[i]..p[es->len - 1].  */
	for (; i < es->len; i++)
		append_normal_char (result, p[i]);
	return 1;
}

/* This function makes two passes over the argument string S.  The first
   one converts all \c and \ddd escapes to their one-byte representations.
   The second constructs a linked specification list, SPEC_LIST, of the
   characters and constructs that comprise the argument string.  If either
   of these passes detects an error, this function returns false.  */
static int
parse_str (char const *s, struct Spec_list *spec_list)
{
	struct E_string es;
	int ok = unquote (s, &es) && build_spec_list (&es, spec_list);
	es_free (&es);
	return ok;
}

/* Return nonzero if the character C is a member of the
   equivalence class containing the character EQUIV_CLASS.  */
static inline int
is_equiv_class_member (unsigned char equiv_class, unsigned char c)
{
	return (equiv_class == c);
}

/* Return true if the character C is a member of the
   character class CHAR_CLASS.  */
static int
is_char_class_member (enum Char_class char_class, unsigned char c)
{
	int result;
	switch (char_class)
	{
	case CC_ALNUM:
		result = isalnum (c);
		break;
	case CC_ALPHA:
		result = isalpha (c);
		break;
	case CC_BLANK:
		result = isblank (c);
		break;
	case CC_CNTRL:
		result = iscntrl (c);
		break;
	case CC_DIGIT:
		result = isdigit (c);
		break;
	case CC_GRAPH:
		result = isgraph (c);
		break;
	case CC_LOWER:
		result = islower (c);
		break;
	case CC_PRINT:
		result = isprint (c);
		break;
	case CC_PUNCT:
		result = ispunct (c);
		break;
	case CC_SPACE:
		result = isspace (c);
		break;
	case CC_UPPER:
		result = isupper (c);
		break;
	case CC_XDIGIT:
		result = isxdigit (c);
		break;
	default:
		abort ();
		break;
	}
	return !! result;
}

/* Given two specification lists, S1 and S2, and assuming that
   S1->length > S2->length, append a single [c*n] element to S2 where c                                                                                                            
   is the last character in the expansion of S2 and n is the difference
   between the two lengths.
   Upon successful completion, S2->length is set to S1->length.  The only
   way this function can fail to make S2 as long as S1 is when S2 has
   zero-length, since in that case, there is no last character to repeat.
   So S2->length is required to be at least 1.

   Providing this functionality allows the user to do some pretty
   non-BSD (and non-portable) things:  For example, the command
       tr -cs '[:upper:]0-9' '[:lower:]'
   is almost guaranteed to give results that depend on your collating
   sequence.  */
static void
string2_extend ( rlm_translate_t *data )
{
	struct List_element *p;
	unsigned char char_to_repeat;
	int i;

	rad_assert (data->translate);
	rad_assert (data->s1->length > data->s2->length);
	rad_assert (data->s2->length > 0);
	p = data->s2->tail;
	switch (p->type)
	{
	case RE_NORMAL_CHAR:
		char_to_repeat = p->u.normal_char;
		break;
	case RE_RANGE:
		char_to_repeat = p->u.range.last_char;
		break;
	case RE_CHAR_CLASS:
		for (i = N_CHARS - 1; i >= 0; i--) {
			if (is_char_class_member (p->u.char_class, i))
				break;
		}
		rad_assert (i >= 0);
		char_to_repeat = i;
		break;
	case RE_REPEATED_CHAR:
		char_to_repeat = p->u.repeated_char.the_repeated_char;
		break;
	case RE_EQUIV_CLASS:
		/* This shouldn't happen, because validate exits with an error
		   if it finds an equiv class in string2 when translating.  */
		abort ();
		break;
	default:
		abort ();
		break;
	}
	append_repeated_char (data->s2, char_to_repeat, data->s1->length - data->s2->length);
	data->s2->length = data->s1->length;
}

/* Gather statistics about the spec-list S in preparation for the tests
   in validate that determine the consistency of the specs.  This function
   is called at most twice; once for string1, and again for any string2.
   LEN_S1 < 0 indicates that this is the first call and that S represents
   string1.  When LEN_S1 >= 0, it is the length of the expansion of the
   constructs in string1, and we can use its value to resolve any
   indefinite repeat construct in S (which represents string2).  Hence,
   this function has the side-effect that it converts a valid [c*]
   construct in string2 to [c*n] where n is large enough (or 0) to give
   string2 the same length as string1.  For example, with the command
   tr a-z 'A[\n*]Z' on the second call to get_spec_stats, LEN_S1 would
   be 26 and S (representing string2) would be converted to 'A[\n*24]Z'.  */
static void
get_spec_stats (struct Spec_list *s)
{
	struct List_element *p;
	count length = 0;
	s->n_indefinite_repeats = 0;
	s->has_equiv_class = 0;
	s->has_restricted_char_class = 0;
	s->has_char_class = 0;
	for (p = s->head->next; p; p = p->next) {
		int i;
		count len = 0;
		count new_length;
		switch (p->type)
		{
		case RE_NORMAL_CHAR:
			len = 1;
			break;
		case RE_RANGE:
			rad_assert (p->u.range.last_char >= p->u.range.first_char);
			len = p->u.range.last_char - p->u.range.first_char + 1;
			break;
		case RE_CHAR_CLASS:
			s->has_char_class = 1;
			for (i = 0; i < N_CHARS; i++)
				if (is_char_class_member (p->u.char_class, i))
					++len;
			switch (p->u.char_class)
			{
			case CC_UPPER:
			case CC_LOWER:
				break;
			default:
				s->has_restricted_char_class = 1;
				break;
			}
			break;
		case RE_EQUIV_CLASS:
			for (i = 0; i < N_CHARS; i++)
				if (is_equiv_class_member (p->u.equiv_code, i))
					++len;
			s->has_equiv_class = 1;
			break;
		case RE_REPEATED_CHAR:
			if (p->u.repeated_char.repeat_count > 0)
				len = p->u.repeated_char.repeat_count;
			else {
				s->indefinite_repeat_element = p;
				++(s->n_indefinite_repeats);
			}
			break;
		default:
			abort ();
			break;
		}
		/* Check for arithmetic overflow in computing length.  Also, reject
		any length greater than the maximum repeat count, in case the
		length is later used to compute the repeat count for an
		indefinite element.  */
		new_length = length + len;
		if (! (length <= new_length && new_length <= REPEAT_COUNT_MAXIMUM)) {
			radlog (L_ERR, "rlm_translate: too many characters in set");
			abort ();
		}
		length = new_length;
	}
	s->length = length;
}

/* Given a Spec_list S (with its saved state implicit in the values
   of its members `tail' and `state'), return the next single character
   in the expansion of S's constructs.  If the last character of S was
   returned on the previous call or if S was empty, this function
   returns -1.  For example, successive calls to get_next where S
   represents the spec-string 'a-d[y*3]' will return the sequence
   of values a, b, c, d, y, y, y, -1.  Finally, if the construct from
   which the returned character comes is [:upper:] or [:lower:], the
   parameter CLASS is given a value to indicate which it was.  Otherwise
   CLASS is set to UL_NONE.  This value is used only when constructing
   the translation table to verify that any occurrences of upper and
   lower class constructs in the spec-strings appear in the same relative
   positions.  */
static int
get_next (struct Spec_list *s, enum Upper_Lower_class *class)
{
	struct List_element *p;
	int return_val;
	int i;

	if (class)
		*class = UL_NONE;
	if (s->state == BEGIN_STATE)
	{
		s->tail = s->head->next;
		s->state = NEW_ELEMENT;
	}
	p = s->tail;
	if (p == NULL)
		return -1;
	switch (p->type)
	{
	case RE_NORMAL_CHAR:
		return_val = p->u.normal_char;
		s->state = NEW_ELEMENT;
		s->tail = p->next;
		break;
	case RE_RANGE:
		if (s->state == NEW_ELEMENT)
			s->state = p->u.range.first_char;
		else
			++(s->state);
		return_val = s->state;
		if (s->state == p->u.range.last_char)
		{
			s->tail = p->next;
			s->state = NEW_ELEMENT;
		}
		break;
	case RE_CHAR_CLASS:
		if (class)
		{
			switch (p->u.char_class)
			{
			case CC_LOWER:
				*class = UL_LOWER;
				break;
			case CC_UPPER:
				*class = UL_UPPER;
				break;
			default:
				break;
			}
		}
		if (s->state == NEW_ELEMENT)
		{
			for (i = 0; i < N_CHARS; i++)
			if (is_char_class_member (p->u.char_class, i))
				break;
			rad_assert (i < N_CHARS);
			s->state = i;
		}
		rad_assert (is_char_class_member (p->u.char_class, s->state));
		return_val = s->state;
		for (i = s->state + 1; i < N_CHARS; i++) {
			if (is_char_class_member (p->u.char_class, i))
				break;
		}
		if (i < N_CHARS)
			s->state = i;
		else
		{
			s->tail = p->next;
			s->state = NEW_ELEMENT;
		}
		break;
	case RE_EQUIV_CLASS:
		/* FIXME: this assumes that each character is alone in its own
		equivalence class (which appears to be correct for my
		LC_COLLATE.  But I don't know of any function that allows
		one to determine a character's equivalence class.  */
		return_val = p->u.equiv_code;
		s->state = NEW_ELEMENT;
		s->tail = p->next;
		break;

	case RE_REPEATED_CHAR:
		/* Here, a repeat count of n == 0 means don't repeat at all.  */
		if (p->u.repeated_char.repeat_count == 0) {
			s->tail = p->next;
			s->state = NEW_ELEMENT;
			return_val = get_next (s, class);
		} else {
			if (s->state == NEW_ELEMENT) {
				s->state = 0;
			}
			++(s->state);
			return_val = p->u.repeated_char.the_repeated_char;
			if (s->state == p->u.repeated_char.repeat_count) {
				s->tail = p->next;
				s->state = NEW_ELEMENT;
			}
		}
		break;
	default:
		abort ();
		break;
	}
	return return_val;
}

/* This is a minor kludge.  This function is called from
   get_spec_stats to determine the cardinality of a set derived
   from a complemented string.  It's a kludge in that some of the
   same operations are (duplicated) performed in set_initialize.  */
static int
card_of_complement (struct Spec_list *s)
{
	int c;
	int cardinality = N_CHARS;
	int in_set[N_CHARS] = { 0, };

	s->state = BEGIN_STATE;
	while ((c = get_next (s, NULL)) != -1) {
		cardinality -= (!in_set[c]);
		in_set[c] = 1;
	}
	return cardinality;
}

static void
get_s1_spec_stats ( rlm_translate_t *data )
{
	get_spec_stats (data->s1);
	if (data->complement)
		data->s1->length = card_of_complement (data->s1);
}

static void
get_s2_spec_stats ( rlm_translate_t *data )
{
	get_spec_stats (data->s2);
	if (data->s1->length >= data->s2->length && data->s2->n_indefinite_repeats == 1) {
		data->s2->indefinite_repeat_element->u.repeated_char.repeat_count = data->s1->length - data->s2->length;
		data->s2->length = data->s1->length;
	}
}

/* Return true if S is a non-empty list in which exactly one
   character (but potentially, many instances of it) appears.
   E.g., [X*] or xxxxxxxx.  */
static int
homogeneous_spec_list (struct Spec_list *s)
{
	int b, c;

	s->state = BEGIN_STATE;
	if ((b = get_next (s, NULL)) == -1)
		return 0;
	while ((c = get_next (s, NULL)) != -1) {
		if (c != b)
			return 0;
	}
	return 1;
}

/* Die with an error message if S1 and S2 describe strings that
   are not valid with the given command line switches.
   A side effect of this function is that if a valid [c*] or
   [c*0] construct appears in string2, it is converted to [c*n]
   with a value for n that makes s2->length == s1->length.  By
   the same token, if the --truncate-set1 option is not
   given, S2 may be extended.  */
static int
validate ( rlm_translate_t *data )
{
	get_s1_spec_stats (data);
	if (data->s1->n_indefinite_repeats > 0) {
		radlog(L_ERR, "[%s]: the [c*] repeat construct may not appear in string1", data->name);
		return 0;
	}
	if (data->s2) {
		get_s2_spec_stats (data);
		if (data->s2->n_indefinite_repeats > 1) {
			radlog (L_ERR, "[%s]: only one [c*] repeat construct may appear in string2", data->name);
			return 0;
		}
		if (data->translate) {
			if (data->s2->has_equiv_class) {
				radlog (L_ERR, "[%s]: [=c=] expressions may not appear in string2 when translating", data->name);
				return 0;
			}
			if (data->s1) {
				if (data->s1->length > data->s2->length) {
					/* string2 must be non-empty unless --truncate-set1 is given or string1 is empty.  */
					if (data->s2->length == 0) {
						radlog (L_ERR, "[%s]: when not truncating set1, string2 must be non-empty", data->name);
						return 0;
					}
					string2_extend (data);
				}
				if (data->complement && data->s1->has_char_class
				&& ! (data->s2->length == data->s1->length && homogeneous_spec_list (data->s2))) {
					radlog (L_ERR, "[%s]: when translating with complemented character classes,\nstring2 must map all characters in the domain to one", data->name);
					return 0;
				}
			}
			if (data->s2->has_restricted_char_class) {
				radlog (L_ERR, "[%s]: when translating, the only character classes that may appear in\nstring2 are `upper' and `lower'", data->name);
				return 0;
			}
		} else {
			/* Not translating.  */
			if (data->s2->n_indefinite_repeats > 0) {
				radlog (L_ERR, "[%s]: the [c*] construct may appear in string2 only when translating", data->name);
				return 0;
			}
		}
	}
	return 1;
}

/* Initialize a boolean membership set, IN_SET, with the character
   values obtained by traversing the linked list of constructs S
   using the function `get_next'.  IN_SET is expected to have been
   initialized to all zeros by the caller.  If COMPLEMENT_THIS_SET
   is true the resulting set is complemented.  */
static void
set_initialize (struct Spec_list *s, int complement_this_set, int *in_set)
{
	int	c;
	size_t	i;

	s->state = BEGIN_STATE;
	while ((c = get_next (s, NULL)) != -1) {
		in_set[c] = 1;
	}
	if (complement_this_set) {
		DEBUG2 ("rlm_translate: complementing in_set");
		for (i = 0; i < N_CHARS; i++)
			in_set[i] = (in_set[i] == 1)? 0: 1;
	}
}

/* Advance past the current construct.
   S->tail must be non-NULL.  */
static void
skip_construct (struct Spec_list *s)                                                                                                                                               
{
	s->tail = s->tail->next;
	s->state = NEW_ELEMENT;
}

static void
setup_xlate ( rlm_translate_t *data )
{
	int i;
	for (i = 0; i < N_CHARS; i++)
		data->xlate[i] = i;
	if (data->complement) {
		int* in_s1 = data->in_delete_set;
		set_initialize (data->s1, 0, in_s1);
		data->s2->state = BEGIN_STATE;
		for (i = 0; i < N_CHARS; i++) {
			if (! in_s1[i]) {
				int ch = get_next (data->s2, NULL);
				rad_assert (ch != -1);
				if (ch == -1)
					break;
				data->xlate[i] = ch;
			}
		}
	} else {
		int  c1, c2;
		int  case_convert = 0;
		enum Upper_Lower_class class_s1;                                                                                                                                         
		enum Upper_Lower_class class_s2;
		data->s1->state = BEGIN_STATE;
		data->s2->state = BEGIN_STATE;
		for (;;) {
			if (case_convert) {
				skip_construct (data->s1);
				skip_construct (data->s2);
				case_convert = 0;
			}
			c1 = get_next (data->s1, &class_s1);
			c2 = get_next (data->s2, &class_s2);
			if ((class_s1 == UL_NONE)
			&&  (class_s2 == UL_LOWER || class_s2 == UL_UPPER)) {
				radlog (L_ERR, "[%s]: misaligned [:upper:] and/or [:lower:] construct", data->name);
				abort ();
			}
			if (class_s1 == UL_LOWER && class_s2 == UL_UPPER) {
				case_convert = 1;
				for (i = 0; i < N_CHARS; i++) {
					if (islower (i))
						data->xlate[i] = toupper (i);
				}
			}
			else if (class_s1 == UL_UPPER && class_s2 == UL_LOWER) {
				case_convert = 1;
				for (i = 0; i < N_CHARS; i++) {
					if (isupper (i))
						data->xlate[i] = tolower (i);
				}
			}
			else if ((class_s1 == UL_LOWER && class_s2 == UL_LOWER)
			||       (class_s1 == UL_UPPER && class_s2 == UL_UPPER)) {
				/* POSIX says the behavior of `tr "[:upper:]" "[:upper:]"'
				 * is undefined.  Treat it as a no-op.  */
			} else {
				if (c1 == -1 || c2 == -1)
					break;
				data->xlate[c1] = c2;
			}
		}
	rad_assert (c1 == -1);
	}
}

/* Modify the specified attribute so that multiple consecutive occurrences
   of the same character in the attribute value are replaced by a single
   occurrence of that character if the character is in the squeeze set.  */
static void
squeeze_filter ( char* buf, rlm_translate_t *data )
{
        enum	{ NOT_A_CHAR = CHAR_MAX + 1 };
        int	char_to_treat = NOT_A_CHAR;
        size_t	i = 0;
        size_t	dst_idx = 0;
        size_t	buf_len;
        int     was_written;
        int     last_char = NOT_A_CHAR;

        buf_len = strlen (buf);
        was_written = 0;
        for (i=0; i<buf_len; i++) {
                char_to_treat = to_uchar (buf[i]);
                if (char_to_treat != last_char)
                        was_written = 0;
                if (! data->in_squeeze_set[ char_to_treat ]) {
                        was_written = 0;
                        buf[dst_idx] = buf[i];
                        dst_idx++;
                }
                else if ( (data->in_squeeze_set[ char_to_treat ]) && (was_written == 0) ) {
                        buf[dst_idx] = buf[i];
                        dst_idx++;
                        was_written = 1;
                }
                last_char = char_to_treat;
        }
        buf[dst_idx] = '\0';
	DEBUG2 ("  [%s]: squeezed to '%s'", data->name, buf);
}

/* Perform the in-place and one-to-one mapping specified by the global
   array `xlate' (translate). */
static void
xlate_filter ( char* buf, rlm_translate_t *data )
{
        size_t i = 0;
        size_t buf_len;

        buf_len = strlen (buf);
        for (i=0; i<buf_len; i++) {
		buf[i] = data->xlate [to_uchar (buf[i])];
	}
        buf[i] = '\0';
	DEBUG2 ("  [%s]: translated to '%s'", data->name, buf);
}

/* Modify the specified attribute so that characters specified in set1
   will be deleted. */
static void
delete_filter ( char* buf, rlm_translate_t *data )
{
        enum   { NOT_A_CHAR = CHAR_MAX + 1 };
        int    char_to_delete = NOT_A_CHAR;
        size_t i = 0;
        size_t dst_idx = 0;
        size_t buf_len;
        buf_len = strlen (buf);
        for (i=0; i<buf_len; i++) {
                char_to_delete = to_uchar (buf[i]);
                if (! data->in_delete_set[ char_to_delete ]) { buf[dst_idx] = buf[i];
                        dst_idx++;
                }
        }
        buf[dst_idx] = '\0';
	DEBUG2 ("  [%s]: deleted to '%s'", data->name, buf);
}

/* Module initialisation */
static int translate_instantiate(CONF_SECTION *conf, void **instance)
{
	rlm_translate_t*	data;
	DICT_ATTR*		dattr;
	/*
	 *	Set up a storage area for instance data
	 */
	data = rad_malloc(sizeof(*data));
	if (!data) {
		return -1;
	}
	memset(data, 0, sizeof(*data));
	/*
	 *	If the configuration parameters can't be parsed, then
	 *	fail.
	 */
	if (cf_section_parse(conf, data, module_config) < 0) {
		free(data);
		return -1;
	}
	/*
	 * 	Add the module instance name 
	 */
	data->name = cf_section_name2(conf); /* may be NULL */
	/*
	 *	Discover the attribute number of the key.
	 */
	if (data->attribute == NULL) {
		radlog(L_ERR, "rlm_translate: 'attribute' must be set.");
		return -1;
	}
	/*
	 *	Check parameters and their combinations
	 */
	if ( (data->translate == 1) && (data->delete == 1) ) {
		radlog(L_ERR, "[%s]: can not delete when translating.", data->name);
		return -1;
	}
	if (data->set1[0] == '\0') {	/* set1 must always be set */
		radlog(L_ERR, "[%s]: set1 must be set.", data->name);
		return -1;
	}
	if ( (data->translate) ||  (data->delete && data->squeeze) ) {
		if (data->set2[0] == '\0') {
			radlog(L_ERR, "[%s]: set2 must be set when translating or (deleting and squeezing)", data->name);
			return -1;
		}
	}
	if (data->searchin_str == NULL) {
		radlog(L_ERR, "[%s]: Illegal searchin directive given. Assuming packet.", data->name);
		data->searchin = RLM_REGEX_INPACKET;
	} else {
		if (strcmp(data->searchin_str, "packet") == 0)
			data->searchin = RLM_REGEX_INPACKET;
		else if (strcmp(data->searchin_str, "config") == 0)
			data->searchin = RLM_REGEX_INCONFIG;
		else if (strcmp(data->searchin_str, "control") == 0)
			data->searchin = RLM_REGEX_INCONFIG;
		else if (strcmp(data->searchin_str, "reply") == 0)
			data->searchin = RLM_REGEX_INREPLY;
		else if (strcmp(data->searchin_str, "proxy") == 0)
			data->searchin = RLM_REGEX_INPROXY;
		else if (strcmp(data->searchin_str, "proxy_reply") == 0)
			data->searchin = RLM_REGEX_INPROXYREPLY;
		else {
			radlog(L_ERR, "[%s]: Illegal searchin directive given. Assuming packet.", data->name);
			data->searchin = RLM_REGEX_INPACKET;
		}
	}
	dattr = dict_attrbyname(data->attribute);
	if (dattr == NULL) {
		radlog(L_ERR, "[%s]: No such attribute %s", data->name, data->attribute);
		return -1;
	}
	data->attr_num = dattr->attr;
	data->set1_len = strlen(data->set1);
	data->s1 = rad_malloc (sizeof (struct Spec_list));
	spec_init (data->s1);
	if (!parse_str (data->set1, data->s1)) {
		radlog(L_ERR, "[%s]: failed to parse '%s' as set1 string.", data->name, data->set1);
		return -1;
	}
	if ((data->translate) || (data->delete)) {
		data->set2_len = strlen(data->set2);
		data->s2 = rad_malloc (sizeof (struct Spec_list));
		spec_init (data->s2);
		if (!parse_str (data->set2, data->s2)) {
			radlog(L_ERR, "[%s]: failed to parse '%s' as set2 string.", data->name, data->set2);
			return -1;
		}
	}
	if (! validate (data))
		return -1;
	*instance = data;
	return 0;
}

static int do_translate(void *instance, REQUEST *request)
{
	int		ret = RLM_MODULE_NOOP;
	char* 		squeeze_chars = 0;
	VALUE_PAIR*	attr_vp = NULL;
	VALUE_PAIR*	tmp = NULL;
	rlm_translate_t*	data = (rlm_translate_t *) instance;

	if ((attr_vp = pairfind(request->config_items, PW_REWRITE_RULE)) != NULL) {
		if (data->name == NULL || strcmp(data->name,attr_vp->vp_strvalue))
			return RLM_MODULE_NOOP;
	}
	switch (data->searchin) {
		case RLM_REGEX_INPACKET:
			if (data->attr_num == PW_USER_NAME)
				attr_vp = request->username;
			else if (data->attr_num == PW_USER_PASSWORD)
				attr_vp = request->password;
			else
				tmp = request->packet->vps;
			break;
		case RLM_REGEX_INCONFIG:
			tmp = request->config_items;
			break;
		case RLM_REGEX_INREPLY:
			tmp = request->reply->vps;
			break;
		case RLM_REGEX_INPROXYREPLY:
			if (!request->proxy_reply)
				return RLM_MODULE_NOOP;
			tmp = request->proxy_reply->vps;
			break;
		case RLM_REGEX_INPROXY:
			if (!request->proxy)
				return RLM_MODULE_NOOP;
			tmp = request->proxy->vps;
			break;
		default:
			radlog(L_ERR, "[%s]: Illegal value for searchin. Changing to packet.", data->name);
			data->searchin = RLM_REGEX_INPACKET;
			attr_vp = pairfind(request->packet->vps, data->attr_num);
			break;
	}
	if (tmp != NULL)
		attr_vp = pairfind(tmp, data->attr_num);
	if (attr_vp == NULL) {
		DEBUG2("[%s]: Could not find value pair for attribute %s", data->name, data->attribute);
		return ret;
	}
	if (attr_vp->vp_strvalue == NULL || attr_vp->length == 0){
		DEBUG2("[%s]: Attribute %s string value NULL or of zero length", data->name, data->attribute);
		return ret;
	}
	if ((attr_vp->type == PW_TYPE_IPADDR) &&
	    (attr_vp->vp_strvalue[0] == '\0')) {
		inet_ntop(AF_INET, &(attr_vp->vp_ipaddr),
			  attr_vp->vp_strvalue,
			  sizeof(attr_vp->vp_strvalue));
	}
	if (data->translate) {
		setup_xlate (data);
		if (data->squeeze) {
			set_initialize (data->s2, data->complement, data->in_squeeze_set);
			squeeze_chars = data->set2;
		}
	} else if (data->delete) {
		set_initialize (data->s1, data->complement, data->in_delete_set);
		if (data->squeeze) {
			set_initialize (data->s2, data->complement, data->in_squeeze_set);
			squeeze_chars = data->set2;
		}
	} else if (data->squeeze) {
		set_initialize (data->s1, data->complement, data->in_squeeze_set);
		squeeze_chars = data->set1;
	}
	if (data->translate) {
		DEBUG2 ("  [%s]: translating %s characters '%s' to '%s' from '%s'",
		  data->name,
		  data->complement? "complement": "the",
		  data->set1,
		  data->set2,
		  attr_vp->vp_strvalue);
		xlate_filter (attr_vp->vp_strvalue, data);
	}
	if (data->delete) {
		DEBUG2 ("  [%s]: deleting %s characters '%s' from '%s'",
		  data->name,
		  data->complement? "complement": "the",
		  data->set1, attr_vp->vp_strvalue);
		delete_filter (attr_vp->vp_strvalue, data);
	}
	if (data->squeeze) {
		DEBUG2 ("  [%s]: squeezing %s characters '%s' from '%s'",
		  data->name,
		  data->complement? "complement": "the",
		  squeeze_chars, attr_vp->vp_strvalue);
		squeeze_filter (attr_vp->vp_strvalue, data);
	}
	return RLM_MODULE_OK;
}

static int translate_accounting(void *instance, REQUEST *request)
{
	return do_translate(instance, request);
}

static int translate_authorize(void *instance, REQUEST *request)
{
	return do_translate(instance, request);
}

static int translate_authenticate(void *instance, REQUEST *request)
{
	return do_translate(instance, request);
}

static int translate_preacct(void *instance, REQUEST *request)
{
	return do_translate(instance, request);
}

static int translate_checksimul(void *instance, REQUEST *request)
{
	return do_translate(instance, request);
}

static int translate_preproxy(void *instance, REQUEST *request)
{
	return do_translate(instance, request);
}

static int translate_postproxy(void *instance, REQUEST *request)
{
	return do_translate(instance, request);
}

static int translate_postauth(void *instance, REQUEST *request)
{
	return do_translate(instance, request);
}

static int translate_detach(void *instance)
{
	free(instance);
	return 0;
}

/*
 *	The module name should be the only globally exported symbol.
 *	That is, everything else should be 'static'.
 *
 *	If the module needs to temporarily modify it's instantiation
 *	data, the type should be changed to RLM_TYPE_THREAD_UNSAFE.
 *	The server will then take care of ensuring that the module
 *	is single-threaded.
 */
module_t rlm_translate = {
	RLM_MODULE_INIT,
	"translate",
	RLM_TYPE_THREAD_UNSAFE,		/* type */
	translate_instantiate,		/* instantiation */
	translate_detach,		/* detach */
	{
		translate_authenticate,	/* authentication */
		translate_authorize, 	/* authorization */
		translate_preacct,	/* preaccounting */
		translate_accounting,	/* accounting */
		translate_checksimul,	/* checksimul */
		translate_preproxy,	/* pre-proxy */
		translate_postproxy,	/* post-proxy */
		translate_postauth	/* post-auth */
	},
};
