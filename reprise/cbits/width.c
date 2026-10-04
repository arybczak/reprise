#define _XOPEN_SOURCE 700

#include <locale.h>
#include <wchar.h>

// A CJK ideograph is wide in every UTF-8 locale, and wcwidth returns -1 for
// it in a locale without UTF-8.
static int utf8_ctype(void)
{
  return wcwidth(0x65E5) == 2;
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
