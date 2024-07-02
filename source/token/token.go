package token

type TokenType string

const (
	// Keywords
	ELSE   = "else"
	EVAL   = "eval"
	GIVEN  = "given"
	GLOBAL = "global"
	GOCODE = "gocode"
	LOOP   = "loop"
	TRY    = "try"
	UNWRAP = "unwrap"
	VALID  = "valid"

	// Headwords
	IMPORT  = "import"
	VAR     = "var"
	CMD     = "cmd"
	DEF     = "def"
	PRIVATE = "private"
	LANG    = "lang"
	EXTERN  = "external"
	CONST   = "const"
	NEWTYPE = "newtype"

	// Special operations
	AND = "and"
	OR  = "or"
	NOT = "not"

	EMDASH  = "---"
	EQ      = "=="
	FILTER  = "?>"
	MAPPING = ">>"
	NOT_EQ  = "!="
	PIPE    = "->"

	// Assignment operators
	ASSIGN     = "="
	GVN_ASSIGN = "=GVN=" // To store hidden data. Probably can be abolished now like the others. TODO.

	// The protected punctuation.
	COLON               = ":"
	DOTDOT              = ".."
	COMMA               = ","
	LPAREN              = "("
	LBRACE              = "{"
	LBRACK              = "["
	NAMESPACE_SEPARATOR = "."
	NEWLINE             = "\n"
	RBRACE              = "}"
	RBRACK              = "]"
	RPAREN              = ")"
	SEMICOLON           = ";"

	// Literals, comments, and identifiers.
	COMMENT = "COMMENT"
	FLOAT   = "FLOAT LITERAL"
	IDENT   = "IDENT"
	INT     = "INTEGER LITERAL"
	STRING  = "STRING LITERAL"
	RUNE    = "RUNE LITERAL"

	// False and true.
	FALSE = "false"
	TRUE  = "true"

	// For internal use.
	BEGIN       = "BEGIN"
	BUILTIN     = "BUILTIN"
	END         = "END"
	EOF         = "EOF"
	IFLOG       = "IFLOG"
	ILLEGAL     = "ILLEGAL"
	LOG         = "LOG"
	MAGIC_COLON = "MAGIC COLON"
	NO_INDENT   = "|||"
	PRELOG      = "PRELOG"
	WEAK_COMMA  = ",,"
	XCALL       = "XCALL"
)

type Token struct {
	Type    TokenType
	Literal string
	Line    int
	ChStart int
	ChEnd   int
	Source  string
}

var keywords = map[string]TokenType{
	// Keywords.
	"else":   ELSE,
	"eval":   EVAL,
	"given":  GIVEN,
	"gocode": GOCODE,
	"global": GLOBAL,
	"loop":   LOOP,
	"try":    TRY,
	"unwrap": UNWRAP,
	"valid":  VALID,

	// Headwords.
	"const":    CONST,
	"external": EXTERN,
	"cmd":      CMD,
	"def":      DEF,
	"import":   IMPORT,
	"lang":     LANG,
	"private":  PRIVATE,
	"newtype":  NEWTYPE,
	"var":      VAR,

	//Special operators.
	"and": AND,
	"or":  OR,
	"not": NOT,
	"->":  PIPE,
	">>":  MAPPING,
	"?>":  FILTER,
	"==":  EQ,
	"!=":  NOT_EQ,
	"---": EMDASH,

	// False and true.
	"true":  TRUE,
	"false": FALSE,

	// Secret sauce.
	"xcall": XCALL,
}

func LookupIdent(ident string) TokenType {
	if tok, ok := keywords[ident]; ok {
		return tok
	}
	return IDENT
}

func TokenTypeIsHeadword(t TokenType) bool {
	return t == IMPORT || t == VAR || t == CMD || t == DEF || t == LANG || t == EXTERN || t == NEWTYPE || t == CONST
}
