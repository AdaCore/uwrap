from langkit.dsl import ASTNode, abstract, Field
from langkit.parsers import Grammar, List, Pick, Or, Opt, Null
from language.lexer import *

@abstract
class TestNode(ASTNode):
    pass

class Entity(TestNode):
    name = Field()
    nested = Field()

class Identifier(TestNode):
    token_node = True

test_grammar = Grammar('main_rule')
G = test_grammar

test_grammar.add_rules(
    main_rule=G.entity_list,
    entity_list=List (G.entity, sep=',', empty_valid=True),
    entity=Entity (G.identifier, Opt ('{', G.entity_list, '}')),
    identifier=Identifier(Token.Identifier)
)