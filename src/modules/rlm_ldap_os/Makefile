
TARGET     = rlm_ldap_os
SRCS       = rlm_ldap_os.c edir_ldapext.c
HEADERS    =
RLM_CFLAGS =    -DHAVE_LDAP_START_TLS -DHAVE_LDAP_INITIALIZE -DHAVE_LDAP_INT_TLS_CONFIG -DLDAP_DEPRECATED -DLDAP_SET_REBIND_PROC_ARGS=3
RLM_LIBS   = -lldap 

include ../rules.mak

$(LT_OBJS): $(HEADERS)
