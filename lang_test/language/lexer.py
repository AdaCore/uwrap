from langkit.lexer import Lexer, LexerToken, Literal, WithText, Pattern, Ignore, WithSymbol, WithTrivia

class Token(LexerToken):
    LBrk       = WithText()
    RBrk       = WithText()
    Comma      = WithText()
    Comment    = WithTrivia()
    Identifier = WithSymbol()

test_lexer = Lexer(Token)

test_lexer.add_rules(
    (Pattern(r"[ \t\r\n]+"), Ignore()),
    (Pattern(r"#(.?)+"),     Token.Comment),
    (Literal("{"), Token.LBrk),
    (Literal("}"), Token.RBrk),
    (Literal(","), Token.Comma),
    (Pattern(r"[a-zA-Z][a-zA-Z0-9_]*"), Token.Identifier),
    
)
