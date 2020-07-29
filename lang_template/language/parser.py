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
   init = Field()

class Command(TemplateNode):
   actions = Field()

class MatchSection(TemplateNode):
   expression = Field()
   actions = Field ()
   alternate_actions = Field()

class PickSection (TemplateNode):
   expression = Field ()
   actions = Field ()

@abstract
class TemplateSection(TemplateNode):
   actions = Field()

class WrapSection(TemplateSection):
   pass

class WeaveSection(TemplateSection):
   pass

class TemplateCall(TemplateNode):
   captured = Field()
   name = Field()
   args = Field()

class ElseSection(TemplateNode):
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
   alternatives = ['and', 'or', 'not', 'amp', 'is', 'has', 'many', 'few', 'plus', 'minus', 'multiply', 'divide']

class BinaryExpr(Expr):
   lhs = Field()
   op = Field()
   rhs = Field()

class UnaryExpr(Expr):
   op = Field()
   rhs = Field()

class Function(Expr):
   name = Field()
   args = Field()
   program = Field()

class Visitor(Expr):
   name = Field()
   args = Field()
   program = Field()

class MatchExpr (Expr):
   match_exp = Field ()
   pick_exp = Field ()
   else_exp = Field ()

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

class TraverseInto (TemplateNode):
   pass

class TraverseOver (TemplateNode):
   pass

class NewExpr(TemplateNode):
   tree=Field()

class FoldExpr(TemplateNode):
   default=Field()
   combine=Field()
   separator=Field()

class AllExpr(TemplateNode):
   expression=Field()

class AtRef (TemplateNode):
   token_node = True

class CreateTemplateTree (TemplateNode):
   root=Field()
   tree=Field()

class QualifiedMatch (TemplateNode):
   op = Field()
   rhs = Field()

class CommandSequence (TemplateNode):
   commands = Field()
   then = Field ()

class RegExpr (TemplateNode):
   left = Field()
   right = Field()

class RegExprAnchor (TemplateNode):
   token_node = True

class RegExprQuantifier (TemplateNode):
   quantifier = Field ()
   expr = Field ()
   min = Field ()
   max = Field ()

template_grammar = Grammar('main_rule')
G = template_grammar

template_grammar.add_rules(
   main_rule=Module (List (G.import_clause, empty_valid=True), G.module_scope),
   import_clause=Import('import', G.dotted_name, ';'),
    
   module_scope=List(Or (G.template, G.command, G.visitor, G.function, G.var), empty_valid=True),
   template_scope=List(G.var, empty_valid=True),

   template=Template('template', G.identifier, Opt ('extends', G.dotted_name), 'do', G.template_scope, 'end', ';'),
    
   var=Var('var', G.identifier, ':', G.identifier, Opt ('(', G.arg_list, ')'), Opt ('=>', G.expression), ';'), 

   command=Command(
      Or(
         G.match_section,
         G.pick_section,
         G.wrap_section, 
         G.weave_section,
         G.command_sequence
      )
   ),
   match_section=MatchSection (
      Pick ('match', G.expression),
      Or (
         G.pick_section,
         G.wrap_section,
         G.weave_section,
         G.command_sequence,
         Pick (Null (G.command_sequence), ';')),
      Opt(Pick ('else', ElseSection(G.command)))
   ),
   pick_section=PickSection (
      Pick ('pick', G.expression),
      Or (
         G.wrap_section,
         G.weave_section,
         G.command_sequence,
         Pick (Null (G.command_sequence), ';'))
   ),
   weave_section=WeaveSection(
      'weave',
      Or (
         G.template_call,
         TemplateCall(Null (G.identifier), Null (G.dotted_name), '(', G.arg_list, ')'),
         G.traverse_decision),
      ';'),
   wrap_section=WrapSection(
      'wrap', 
      Or (
         G.template_call,
         G.traverse_decision), 
      ';'),

   command_sequence=Pick ('do', G.command_sequence_element),
   command_sequence_element=CommandSequence (
      List (G.command, empty_valid = True), Or (
         Pick ('then', G.command_sequence_element),
         Pick ('end', Null (G.command_sequence_element), ';'))),
   
   traverse_decision=Or(TraverseInto ('into'), TraverseOver ('over')),

   visitor=Visitor('visitor', G.identifier, '(', Opt (List (G.identifier, sep = ',', empty_valid = True)), ')', G.command_sequence),
   function=Function('function', G.identifier, '(', Opt (List (G.identifier, sep = ',', empty_valid = True)), ')', G.command_sequence),
   
   root_expression=Or (      
      RegExpr (
         RegExprAnchor ('\\'), 
         G.regular_expression),
      G.regular_expression_with_suffix,
      G.expression),
   regular_expression_with_suffix=RegExpr (
      Or (
         G.regular_expression_quantifier,
         G.expression),
      Or (Pick ('\\', G.regular_expression),
         RegExprAnchor ('\\'))),      
   regular_expression=RegExpr (
      Or (
         G.regular_expression_quantifier,
         G.expression),
      Opt (
         Or (Pick ('\\', G.regular_expression),
             RegExprAnchor ('\\')))),
   regular_expression_quantifier=RegExprQuantifier (
      Or (Operator.alt_many('many'), Operator.alt_few('few')),
      '(', 
      G.expression, 
      Opt (Pick (',', G.integer)),
      Opt (Pick (',', G.integer)),
      ')'), 
   expression=Or (
      BinaryExpr (G.relation, Or (Operator.alt_and('and'), Operator.alt_or('or')), G.expression),
      G.relation),
   relation=G.simple_expression,
   simple_expression=Or (BinaryExpr (G.term, Or (Operator.alt_amp('&'), Operator.alt_minus('-'), Operator.alt_plus('+')), G.simple_expression), G.term),
   term=Or (BinaryExpr (G.factor, Or (Operator.alt_multiply('*'), Operator.alt_divide('/')), G.term), G.factor),
   factor=Or(
      MatchCapture(G.identifier, ':', G.factor),
      UnaryExpr (Operator.alt_not('not'), G.qualified_primary), 
      G.qualified_primary),
   qualified_primary=Or (QualifiedMatch (Or (Operator.alt_is('is'), Operator.alt_has ('has')), '(', G.primary, ')'), G.primary),
   primary=Or(
      Pick ('(', G.expression, ')'),
      G.match_expr,
      G.lambda_expr,
      G.literal,
      G.integer,
      G.str,
      G.selected_component,
      G.name
   ),
   match_expr=Pick ('(', G.match_expr_element, ')'),
   match_expr_element=MatchExpr (
      'match', G.expression, 'pick', G.expression,
      Opt ('else', Or (
         Pick ('pick', G.expression),
         G.match_expr_element))),
   name=Or(
      G.single_name
   ),
   single_name=Or (
      G.at_ref, 
      G.new_expr, 
      G.fold_expr,
      G.all_expr,
      G.call_expr,
      G.identifier),
   selected_component=Selector (G.prefix, '.', G.suffix), 
   prefix=Or (  
      G.name
   ),
   suffix=Or (
      G.selected_component,
      G.name
   ),
   new_expr=NewExpr ('new', '(', G.create_template_tree, ')'),
   template_call=TemplateCall(Opt (G.identifier, ':'), G.dotted_name, '(', G.arg_list, ')'),
   create_template_tree=Or(
      CreateTemplateTree(G.template_call, Opt ('{', List (G.create_template_tree, sep = ',', empty_valid = True), '}')),
      CreateTemplateTree(Null (G.template_call), '{', List (G.create_template_tree, sep = ',', empty_valid = True), '}')),
   fold_expr=FoldExpr ('fold', '(', G.expression, ',', G.expression, Opt (',', G.expression), ')'),
   all_expr=AllExpr ('all', '(', Opt (G.expression), ')'),
   at_ref=AtRef('@'),
   call_expr=CallExpr (G.identifier, '(', G.arg_list, ')'),
   lambda_expr=LambdaExpr ('lambda', '(', G.expression, ')'),
   arg_list=List(Argument(Opt (Or (G.identifier, TokenIdentifier ("match")), "=>"), G.root_expression), sep=',', empty_valid=True),
   identifier=Identifier(Token.Identifier),
   dotted_name=Selector(Opt (G.dotted_name, '.'), G.identifier),
   integer=Number(Token.Integer),
   literal=Literal(Or ("true", "false")),
   str=Str(Token.String)
)
