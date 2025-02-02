package token

type TokenType string

const (
	// Keywords
	BREAK    = "break"
	CONTINUE = "continue"
	ELSE     = "else"
	EVAL     = "eval"
	FOR      = "for"
	GIVEN    = "given"
	GLOBAL   = "global"
	GOCODE   = "golang"
	RANGE    = "range"
	TRY      = "try"
	UNWRAP   = "unwrap"
	VALID    = "valid"

	// Headwords
	IMPORT  = "import"
	VAR     = "var"
	CMD     = "cmd"
	DEF     = "def"
	PRIVATE = "private"
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
	DOTDOTDOT           = "..."
	COMMA               = ","
	LPAREN              = "("
	LBRACE              = "{"
	LBRACK              = "["
	NAMESPACE_SEPARATOR = "."
	NEWLINE             = "\n"
	RBRACE              = "}"
	RBRACK              = "]"
	RPAREN              = ")"
	SEMICOLON           = "SEMICOLON"

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
	BEGIN           = "BEGIN"           // What we turn an indent into.
	BUILTIN         = "BUILTIN"         // Used in the 'builtins.pf' file to supply hoods to the builtings.
	END             = "END"             // What we turn an outdent into.
	EOF             = "EOF"             // End of file.
	IFLOG           = "IFLOG"           // What the relexer turns ': \\' into.
	ILLEGAL         = "ILLEGAL"         // The type for the lexer to return when it is perp-lexed.
	LOG             = "LOG"             // What we turn \\ into.
	MAGIC_COLON     = "MAGIC COLON"     // What the relexer turns a colon into when it comes after the signature of an inner function.
	MAGIC_SEMICOLON = "MAGIC_SEMICOLON" // What the semicolons in C-like for loops get turned into by the relexer.
	NO_INDENT       = "|||"             // What we turn whitespace into when it isn't a new indent or outdent.
	PRELOG          = "PRELOG"          // What we turn \\ into when it's the first thing after the function signature.
	WEAK_COMMA      = ",,"              // What we turn commas after type names into in function signatures.
	XCALL           = "XCALL"           // Used in generated code to supply hooks to the external calls.
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
	"break":    BREAK,
	"continue": CONTINUE,
	"else":     ELSE,
	"eval":     EVAL,
	"for":      FOR,
	"given":    GIVEN,
	"golang":   GOCODE,
	"global":   GLOBAL,
	"range":    RANGE,
	"try":      TRY,
	"unwrap":   UNWRAP,
	"valid":    VALID,

	// Headwords.
	"cmd":      CMD,
	"const":    CONST,
	"def":      DEF,
	"external": EXTERN,
	"import":   IMPORT,
	"newtype":  NEWTYPE,
	"private":  PRIVATE,
	"var":      VAR,

	//Special operators.
	"and": AND,
	"not": NOT,
	"or":  OR,
	"->":  PIPE,
	">>":  MAPPING,
	"?>":  FILTER,
	"==":  EQ,
	"!=":  NOT_EQ,
	"--":  EMDASH,

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
	return t == IMPORT || t == VAR || t == CMD || t == DEF || t == EXTERN || t == NEWTYPE || t == CONST
}
