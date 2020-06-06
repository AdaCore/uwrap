from langkit.dsl import ASTNode, abstract, Field
from langkit.parsers import Grammar, List, Pick, Or, Opt, Null
from language.lexer import *


@abstract
class TemplateNode(ASTNode):
    """
    Root node class for Wraplang AST nodes.
    """
    pass

class Import(TemplateNode):
    name = Field()

class Module(TemplateNode):
    import_clauses = Field()
    program = Field()

class Template(TemplateNode):
    name = Field()
    extending = Field()
    definition = Field()

class Var(TemplateNode):
    name = Field()
    typ = Field()
    args = Field()

class Command(TemplateNode):
    match_clause = Field()
    actions = Field()

class SingleCommand(TemplateNode):
    command = Field()
    alternate_actions = Field()

class BlockCommand(TemplateNode):
    commands = Field()
    alternate_actions = Field()

class MatchClause(TemplateNode):
    expression = Field()

@abstract
class TemplateClause(TemplateNode):
    target = Field()
    template_set = Field()

class WrapClause(TemplateClause):
    pass

class WeaveClause(TemplateClause):
    pass

class TemplateSet(TemplateNode):
    name = Field()
    args = Field()

class ApplyClause(TemplateNode):
    expression = Field()

class ElseClause(TemplateNode):
    actions = Field()

class MatchCapture(TemplateNode):
    captured = Field()
    expression = Field()

class TokenTemplate(TemplateNode):
    token_node = True

@abstract
class Expr(TemplateNode):
    pass

class Number(Expr):
    token_node = True

class DottedName(Expr):
    prefix = Field()
    suffix = Field()

class Identifier(Expr):
    token_node = True

class Literal (Expr):
    token_node = True

class Str(Expr):
    token_node = True

class Operator(TemplateNode):
    enum_node = True
    alternatives = ['and', 'or', 'not']

class BinaryExpr(Expr):
    lhs = Field()
    op = Field()
    rhs = Field()

class UnaryExpr(Expr):
    op = Field()
    rhs = Field()

class CommandFunction(Expr):
    name = Field()
    args = Field()
    commands = Field()

class CallExpr (TemplateNode):
    called = Field()
    args = Field()

class Argument(TemplateNode):
    name = Field()
    value = Field()

class Selector(TemplateNode):
    left=Field()
    right=Field()

class NestedScope (TemplateNode):
    scope=Field()

class TraverseClause (TemplateNode):
    actions=Field()

class TraverseSequence (TemplateNode):
    first=Field()
    end=Field()

class TraverseStep(TemplateNode):
    expression=Field()
    into=Field()
    then=Field()

class TraverseEndInto (TemplateNode):
    call=Field()

class TraverseEndOver (TemplateNode):
    pass

class NoNode(TemplateNode):
    pass

template_grammar = Grammar('main_rule')
G = template_grammar

template_grammar.add_rules(
    main_rule=Module (List (G.import_clause, empty_valid=True), G.module_scope),
    import_clause=Import('import', G.dotted_name, ';'),
    
    module_scope=List(Or (G.template, G.command, G.command_function, G.var, NestedScope ('{', G.module_scope, '}')), empty_valid=True),
    command_scope=List(Or (G.command, G.command_function, G.var, NestedScope ('{', G.command_scope, '}')), empty_valid=True),
    template_scope=List(G.var, empty_valid=True),

    template=Template('template', G.identifier, Opt ('extends', G.dotted_name), '{', G.template_scope, '}'),
    
    var=Var('var', G.identifier, ':', G.identifier, Opt ('(', G.arg_list, ')'), ';'), 

    command=Command(
        Opt(G.match_clause),
        Or(G.single_command, G.block_command)),
    single_command=SingleCommand(
        Or(
            Pick (G.wrap_clause),
            Pick (G.weave_clause),
            Pick (G.apply_clause),
            Pick (G.traverse_clause)
        ),
        Or (
            Pick (';', NoNode()),
            Pick ('else', ElseClause(Or (Pick (G.nested_commands), G.command)))
        ),
    ),
    block_command=BlockCommand(
        G.nested_commands,
        Opt ('else', ElseClause(Or (G.nested_commands, G.command)))
    ),
    command_function=CommandFunction('command', G.identifier, '(', Opt (List (G.identifier)), ')', G.nested_commands),
    nested_commands=NestedScope ('{', G.command_scope, '}'),
    match_clause=MatchClause('match', G.match_expr),
    wrap_clause=WrapClause('wrap', Opt (G.expr, 'with'), TemplateSet (G.dotted_name, '(', G.arg_list, ')')),
    weave_clause=WeaveClause('weave', Opt (G.expr, 'with'), TemplateSet (G.dotted_name, '(', G.arg_list, ')')),
    apply_clause=ApplyClause('apply', G.call_expr),

    traverse_clause=TraverseClause('traverse', Or (G.traverse_sequence, G.traverse_end)),
    traverse_sequence=TraverseSequence(
        G.traverse_step,
        Opt ('then', G.traverse_end)),
    traverse_step=TraverseStep(
        G.expr, 
        Opt ('into', G.call_expr), 
        Opt ('then', G.traverse_step)),
    traverse_end=Opt(Or(TraverseEndInto ('into', Opt (G.call_expr)), TraverseEndOver ('over'))),

    match_expr=Or(
        Pick('(', G.match_expr, ')'),
        MatchCapture(G.identifier, ':', G.match_expr),
        UnaryExpr(Operator.alt_not('not'), G.match_expr),
        BinaryExpr(G.match_expr,
            Or(Operator.alt_and('and'),
               Operator.alt_or('or')),
               G.match_expr),
        Selector (G.match_expr, '.', G.match_expr),
        G.match_call_or_single
    ),
    match_call_or_single=Or(
        G.match_call_expr,
        G.call_or_single
    ),
    match_call_expr=CallExpr (G.identifier, '(', G.match_arg_list, ')'),
    match_arg_list = List(
        Argument(
            Opt (G.identifier, "=>"), G.match_expr), 
        sep=',', empty_valid=True
    ),
    expr=Or(
       Pick('(', G.expr, ')'),
       UnaryExpr(Operator.alt_not('not'), G.expr),
       BinaryExpr(G.expr,
            Or(Operator.alt_and('and'),
              Operator.alt_or('or')),
            G.expr),
       Selector (G.expr, '.', G.expr),
       G.call_or_single
   ),
   call_or_single=Or(
    G.call_expr,
    G.identifier,
    G.literal,
    G.integer,
    G.str
    ),
   call_expr=CallExpr (G.identifier, '(', G.arg_list, ')'),
   arg_list=List(Argument(Opt (G.identifier, "=>"), G.expr), sep=',', empty_valid=True),
   identifier=Or (TokenTemplate ('template'), Identifier(Token.Identifier)),
   dotted_name=DottedName(Opt (G.dotted_name, '.'), G.identifier),
   integer=Number(Token.Integer),
   literal=Literal(Or ("true", "false")),
   str=Str(Token.String)
)