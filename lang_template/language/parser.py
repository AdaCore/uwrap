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

class TokenIdentifier(TemplateNode):
    token_node = True

@abstract
class Expr(TemplateNode):
    pass

class Number(Expr):
    token_node = True

class Identifier(Expr):
    token_node = True

class Literal (Expr):
    token_node = True

class Str(Expr):
    token_node = True

class Operator(TemplateNode):
    enum_node = True
    alternatives = ['and', 'or', 'not', 'amp', 'is', 'has']

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

class LambdaExpr (TemplateNode):
    expression = Field()

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

class EntityReference(TemplateNode):
    value=Field()

class TreeReference(EntityReference):
    pass

class TemplateOperation(TemplateNode):
    entity=Field()
    call=Field()

class NewExpr(TemplateNode):
    tree=Field()

class FoldExpr(TemplateNode):
    default=Field()
    combine=Field()

class AtRef (TemplateNode):
    token_node = True

class CreateTemplateTree (TemplateNode):
    captured=Field()
    root=Field()
    tree=Field()

class QualifiedMatch (TemplateNode):
    op = Field()
    rhs = Field()

template_grammar = Grammar('main_rule')
G = template_grammar

template_grammar.add_rules(
    main_rule=Module (List (G.import_clause, empty_valid=True), G.module_scope),
    import_clause=Import('import', G.dotted_name, ';'),
    
    module_scope=List(Or (G.template, G.command, G.visitor, G.var, NestedScope ('{', G.module_scope, '}')), empty_valid=True),
    command_scope=List(Or (G.command, NestedScope ('{', G.command_scope, '}')), empty_valid=True),
    template_scope=List(G.var, empty_valid=True),

    template=Template('template', G.identifier, Opt ('extends', G.dotted_name), '{', G.template_scope, '}'),
    
    var=Var('var', G.identifier, ':', G.identifier, Opt ('(', G.arg_list, ')'), ';'), 

    command=Command(
        Opt(G.match_clause),
        Or(
            Pick (G.wrap_clause),
            Pick (G.weave_clause),
            G.nested_commands
        ),
        Opt (
            Pick ('else', ElseClause(G.command))
        ),
    ),
    visitor=Visitor('visitor', G.identifier, '(', Opt (List (G.identifier, sep = ',', empty_valid = True)), ')', G.nested_commands),
    nested_commands=NestedScope ('{', G.command_scope, '}'),
    match_clause=MatchClause('match', G.expression),
    wrap_clause=WrapClause('wrap', G.template_operation_generic, ';'),
    weave_clause=WeaveClause('weave', G.template_operation_generic, ';'),
    template_operation_generic=Or (
        G.template_operation,
        G.traverse_decision),
    template_operation=
        Or(
            TemplateOperation(G.template_target_expression, Opt (G.template_call_expression)),
            TemplateOperation(Null (G.template_target_expression), G.template_call_expression)),
    template_target_expression=Or(
            TreeReference('all', Opt (G.expression)),
            EntityReference (G.expression)),
    template_call_expression=TemplateCall('with', Opt (G.dotted_name), '(', G.arg_list, ')'),
    traverse_decision=Or(TraverseInto ('into'), TraverseOver ('over')),

    expression=Or (
        BinaryExpr (G.relation, Or (Operator.alt_and('and'), Operator.alt_or('or')), G.expression),
        G.relation),
    relation=G.simple_expression,
    simple_expression=Or (BinaryExpr (G.term, Operator.alt_amp('&'), G.simple_expression), G.term),
    term=G.factor,
    factor=Or(
        MatchCapture(G.identifier, ':', G.factor),
        UnaryExpr (Operator.alt_not('not'), G.qualified_primary), 
        G.qualified_primary),
    qualified_primary=Or (QualifiedMatch (Or (Operator.alt_is('is'), Operator.alt_has ('has')), '\'', G.primary), G.primary),
    primary=Or(
     Pick ('(', G.expression, ')'),
     G.lambda_expr,
     G.literal,
     G.integer,
     G.str,
     G.name,
     ),
    name=Or (G.selected_component, G.at_ref, G.new_expr, G.fold_expr, G.call_expr, G.identifier),
    selected_component=Selector (G.prefix, '.', G.name), # TODO should name be selector name instead?
    #selector_name=Or (G.call_expr, G.identifier),
    prefix=G.name,

    new_expr=NewExpr ('new', '(', G.create_template_tree, ')'),
    template_call=TemplateCall(G.dotted_name, '(', G.arg_list, ')'),
    create_template_tree=Or(
       CreateTemplateTree(Opt (G.identifier, ':'), G.template_call, Opt ('[', List (G.create_template_tree, sep = ',', empty_valid = True), ']')),
       CreateTemplateTree(Opt (G.identifier, ':'), Null (G.template_call), '[', List (G.create_template_tree, sep = ',', empty_valid = True), ']')),
    fold_expr=FoldExpr ('fold', '(', G.expression, ',', G.expression, ')'),
    at_ref=AtRef('@'),
    call_expr=CallExpr (G.identifier, '(', G.arg_list, ')'),
    lambda_expr=LambdaExpr ('lambda', '(', G.expression, ')'),
    arg_list=List(Argument(Opt (G.identifier, "=>"), G.expression), sep=',', empty_valid=True),
    identifier=Or (TokenIdentifier ('template'), TokenIdentifier ('new'), Identifier(Token.Identifier)),
    dotted_name=Selector(Opt (G.dotted_name, '.'), G.identifier),
    integer=Number(Token.Integer),
    literal=Literal(Or ("true", "false")),
    str=Str(Token.String)
)