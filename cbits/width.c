#define _XOPEN_SOURCE 700

#include <langinfo.h>
#include <locale.h>
#include <string.h>
#include <wchar.h>

// Whether the character type locale is of UTF-8. A wide CJK ideograph
// doesn't tell, as it is also wide in a CJK locale without UTF-8, e.g.
// ja_JP.eucJP, whose widths of other characters are its own.
static int utf8_ctype(void)
{
  return strcmp(nl_langinfo(CODESET), "UTF-8") == 0;
}

// Switch the character type locale to UTF-8, so that wcwidth knows the
// widths of all characters: the user's locale if it uses UTF-8, else
// C.UTF-8. Returns 1 on success.
int reprise_use_utf8_ctype(void)
{
  if (setlocale(LC_CTYPE, "") != NULL && utf8_ctype())
    return 1;
  if (setlocale(LC_CTYPE, "C.UTF-8") != NULL && utf8_ctype())
    return 1;
  setlocale(LC_CTYPE, "C");
  return 0;
}

int reprise_wcwidth(int c)
{
  return wcwidth((wchar_t)c);
}
