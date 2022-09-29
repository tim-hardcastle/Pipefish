import

"lib/prelude.ch" :: ""

def

isPalindrome(s) :
    palindrome toLowerCaseAlphanumeric s, ""

palindrome(s) :
    len s == 0 or len s == 1 :
        true
    else :
        (s[0] == s[len(s) - 1]) and palindrome s[1::len(s) - 1]

toLowerCaseAlphanumeric(s, t) :
    s == "" : t
    codepoint s[0] >= 65 and codepoint s[0] <= 90 :
        toLowerCaseAlphanumeric ((s behead 1), t + (rune codepoint(s[0]) + 32))
    codepoint s[0] >= 97 and codepoint s[0] <= 122 :
        toLowerCaseAlphanumeric ((s behead 1), t + s[0])
    codepoint s[0] >= 48 and codepoint s[0] <= 57 :
        toLowerCaseAlphanumeric ((s behead 1), t + s[0])
    else :
        toLowerCaseAlphanumeric ((s behead 1), t)
