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
   command = Field()

class Var(TemplateNode):
   name = Field()
   typ = Field()
   args = Field()
   init = Field()

class Command(TemplateNode):
   actions = Field()

class DeferSection (TemplateNode):
   expression = Field()
   actions = Field ()

class MatchSection(TemplateNode):
   expression = Field()
   actions = Field ()

class PickSection (TemplateNode):
   expression = Field ()
   actions = Field ()

@abstract
class TemplateSection(TemplateNode):
   actions = Field(type=TemplateNode)

class WrapSection(TemplateSection):
   pass

class WeaveSection(TemplateSection):
   pass

class WalkSection(TemplateSection):
   pass

class TemplateCall(TemplateNode):
   captured = Field()
   name = Field()
   args = Field()

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
   alternatives = [
      'and',
      'or',
      'not',
      'amp',
      'is', 
      'has', 
      'many', 
      'few', 
      'plus', 
      'minus', 
      'multiply', 
      'divide',
      'eq',
      'neq',
      'lt',
      'gt',
      'lte',
      'gte']

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

class MatchExpr (Expr):
   match_exp = Field ()
   pick_exp = Field ()
   else_exp = Field ()

class CallExpr (TemplateNode):
   called = Field()
   args = Field()

class DeferExpr (TemplateNode):
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

class FilterExpr(TemplateNode):
   expression=Field()

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
   sequence = Field()

class CommandSequenceElement (TemplateNode):
   vars = Field()
   commands = Field()
   next = Field(type=TemplateNode)

class ThenSequence (TemplateNode):
   actions = Field()

class ElsmatchSequence (TemplateNode):
   expression = Field()
   actions = Field()

class ElseSequence (TemplateNode):
   actions = Field()

class RegExpr (TemplateNode):
   captured = Field ()
   left = Field(type=TemplateNode)
   right = Field(type=TemplateNode)

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
    
   module_scope=List(Or (G.template, G.command, G.function, G.var), empty_valid=True),

   template=Template('template', G.identifier, Opt ('extends', G.dotted_name), Or (G.command, Pick (Null (G.command), ';'))),
    
   var=Var('var', G.identifier, ':', G.identifier, Opt ('(', G.arg_list, ')'), Opt ('=>', G.expression), ';'), 

   command=Command(
      Or(
         G.defer_section,
         G.match_section,
         G.pick_section,
         G.wrap_section, 
         G.weave_section,
         G.walk_section,
         G.command_sequence
      )
   ),
   defer_section=DeferSection (
      'defer', Opt (G.expression),
      Or(
         G.match_section,
         G.pick_section,
         G.wrap_section, 
         G.weave_section,
         G.walk_section,
         G.command_sequence
      )
   ),
   match_section=MatchSection (
      Pick ('match', G.expression),
      Or (
         G.pick_section,
         G.wrap_section,
         G.weave_section,
         G.walk_section,
         G.conditionned_command_sequence,
         Pick (Null (G.command_sequence), ';'))
   ),
   pick_section=PickSection (
      Pick ('pick', G.expression),
      Or (
         G.wrap_section,
         G.weave_section,
         G.walk_section,
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
   walk_section=WalkSection('walk', G.template_call, ';'),
   command_sequence=CommandSequence ('do', G.command_sequence_element, 'end', ';'),
   command_sequence_element=CommandSequenceElement (
      List (G.var, empty_valid = True), 
      List (G.command, empty_valid = True), 
      Opt (ThenSequence ('then', G.command_sequence_element))
   ),
   conditionned_command_sequence=CommandSequence ('do', G.conditionned_command_sequence_element, 'end', ';'),
   conditionned_command_sequence_element=CommandSequenceElement (
      List (G.var, empty_valid = True), 
      List (G.command, empty_valid = True), 
      Opt (Or (
         ThenSequence ('then', G.conditionned_command_sequence_element),
         ElsmatchSequence ('elsmatch', G.expression, 'do', G.conditionned_command_sequence_element),
         ElseSequence ('else', G.command_sequence_element)))
   ),
   traverse_decision=Or(TraverseInto ('into'), TraverseOver ('over')),

   function=Function('function', G.identifier, '(', Opt (List (G.identifier, sep = ',', empty_valid = True)), ')', G.command_sequence),
   
   root_expression=Or (   
      RegExpr (
         Null (G.identifier),
         RegExprAnchor ('\\'), 
         G.regular_expression),
      G.regular_expression),
   regular_expression=Or(
      RegExpr (
         Opt (Pick (G.identifier, ':')),
         Or (
            Pick ('(', G.regular_expression_no_terminal, ')'),
            G.regular_expression_quantifier,
         ),
         Opt (Or (Pick ('\\', G.regular_expression), RegExprAnchor ('\\'))),
      ),
      RegExpr (
         Null (G.identifier),
         G.expression,
         Or (Pick ('\\', G.regular_expression), RegExprAnchor ('\\')),
      ),
      G.expression
   ),
   regular_expression_no_terminal=Or(
      RegExpr (
         Opt (Pick (G.identifier, ':')),
         Or (
            Pick ('(', G.regular_expression_no_terminal, ')'),
            G.regular_expression_quantifier
         ),
         Opt (Pick ('\\', Or (G.regular_expression_no_terminal, G.expression))),
      ),
      RegExpr (
         Null (G.identifier),
         G.expression,
         Pick ('\\', Or (G.regular_expression_no_terminal, G.expression)),
      ),
   ),
   regular_expression_quantifier=RegExprQuantifier (
      Or (Operator.alt_many('many'), Operator.alt_few('few')),
      '(', 
      Or (G.regular_expression_no_terminal, G.expression), 
      Opt (Pick (',', G.integer)),
      Opt (Pick (',', G.integer)),
      ')'), 
   expression=Or (
      BinaryExpr (G.relation, Or (Operator.alt_and('and'), Operator.alt_or('or')), G.expression),
      G.relation),
   relation=Or (
      BinaryExpr (G.simple_expression, Or (
         Operator.alt_eq('='), 
         Operator.alt_neq('/='), 
         Operator.alt_lt('<'), 
         Operator.alt_gt('>'), 
         Operator.alt_lte('<='), 
         Operator.alt_gte('>=')), G.simple_expression),
      G.simple_expression),
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
      G.defer_expr,
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
      G.filter_expr,
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
   filter_expr=FilterExpr ('filter', '(', G.root_expression, ')'),
   all_expr=AllExpr ('all', '(', Opt (G.expression), ')'),
   at_ref=AtRef('@'),
   call_expr=CallExpr (G.identifier, '(', G.arg_list, ')'),
   defer_expr=DeferExpr ('defer', '(', G.expression, ')'),
   arg_list=List(Argument(Opt (G.identifier, "=>"), G.root_expression), sep=',', empty_valid=True),
   identifier=Identifier(Token.Identifier),
   dotted_name=Selector(Opt (G.dotted_name, '.'), G.identifier),
   integer=Number(Token.Integer),
   literal=Literal(Or ("true", "false")),
   str=Str(Token.String)
)
