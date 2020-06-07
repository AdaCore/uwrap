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
    action=Field()

class WrapClause(TemplateClause):
    pass

class WeaveClause(TemplateClause):
    pass

class TemplateCall(TemplateNode):
    name = Field()
    args = Field()

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

class Visitor(Expr):
    name = Field()
    args = Field()
    program = Field()

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

class TraverseInto (TemplateNode):
    pass

class TraverseOver (TemplateNode):
    pass

class NoNode(TemplateNode):
    pass

class EntityReference(TemplateNode):
    value=Field()

class TreeReference(EntityReference):
    pass

class TemplateOperation(TemplateNode):
    entity=Field()
    call=Field()

template_grammar = Grammar('main_rule')
G = template_grammar

template_grammar.add_rules(
    main_rule=Module (List (G.import_clause, empty_valid=True), G.module_scope),
    import_clause=Import('import', G.dotted_name, ';'),
    
    module_scope=List(Or (G.template, G.command, G.visitor, NestedScope ('{', G.module_scope, '}')), empty_valid=True),
    command_scope=List(Or (G.command, NestedScope ('{', G.command_scope, '}')), empty_valid=True),
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
    visitor=Visitor('visitor', G.identifier, '(', Opt (List (G.identifier, sep = ',', empty_valid = True)), ')', G.nested_commands),
    nested_commands=NestedScope ('{', G.command_scope, '}'),
    match_clause=MatchClause('match', G.match_expr),
    wrap_clause=WrapClause('wrap', G.template_operation_generic),
    weave_clause=WeaveClause('weave', G.template_operation_generic),
    template_operation_generic=Or (
        G.template_operation,
        G.traverse_decision),
    template_operation=
        Or(
            TemplateOperation(G.template_target_expression, Opt (G.template_call_expression)),
            TemplateOperation(Opt (G.template_target_expression), G.template_call_expression)),
    template_target_expression=Or(
            TreeReference('all', Opt (G.expr)),
            EntityReference (G.expr)),
    template_call_expression=TemplateCall('with', Opt (G.dotted_name), '(', G.arg_list, ')'),
    traverse_decision=Or(TraverseInto ('into'), TraverseOver ('over')),

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