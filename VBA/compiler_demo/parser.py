import inspect

import pyparsing as pp
from pyparsing import pyparsing_common as ppc

from .ast import *


def _make_parser():
    DIM = pp.Keyword('Dim')
    IF = pp.Keyword('If')
    WHILE = pp.Keyword('While')
    FOR = pp.Keyword('For')
    NEXT = pp.Keyword('Next')
    DO_WHILE = pp.Keyword('Do While')
    LOOP = pp.Keyword('Loop')
    SELECT = pp.Keyword('Select')
    CASE = pp.Keyword('Case')
    CASE_ELSE = pp.Keyword('Case Else')
    RETURN = pp.Keyword('Return')
    END = pp.Keyword('End')
    FUNC = pp.Keyword('Function')
    keywords = IF | FOR | RETURN | WHILE | DO_WHILE | CASE | SELECT

    # num = ppc.fnumber.copy().setParseAction(lambda s, loc, tocs: tocs[0])
    num = pp.Regex('[+-]?\\d+\\.?\\d*([eE][+-]?\\d+)?')
    # c escape-последовательностями как-то неправильно работает
    str_ = pp.QuotedString('"', escChar='\\', unquoteResults=False, convertWhitespaceEscapes=False)
    literal = num | str_ | pp.Regex('True|False')
    # только, чтобы показать, ~keywords здесь не нужен
    ident = (~keywords + ppc.identifier.copy()).setName('ident')
    type_ = ident.copy().setName('type')

    LPAR, RPAR = pp.Literal('(').suppress(), pp.Literal(')').suppress()
    LBRACK, RBRACK = pp.Literal("[").suppress(), pp.Literal("]").suppress()
    LBRACE, RBRACE = pp.Literal("{").suppress(), pp.Literal("}").suppress()
    AS, TO = pp.Literal('As').suppress(), pp.Literal('To').suppress()
    SEMI, COMMA, DOUBLE_ = pp.Literal('\n').suppress(), pp.Literal(',').suppress(), pp.Literal(':').suppress()
    ASSIGN = pp.Literal('=')

    ADD, SUB = pp.Literal('+'), pp.Literal('-')
    MUL, DIV, MOD = pp.Literal('*'), pp.Literal('/'), pp.Literal('Mod')
    AND = pp.Literal('AndAlso')
    OR = pp.Literal('OrElse')
    BIT_AND = pp.Literal('And')
    BIT_OR = pp.Literal('Or')
    GE, LE, GT, LT = pp.Literal('>='), pp.Literal('<='), pp.Literal('>'), pp.Literal('<')
    NEQUALS, EQUALS = pp.Literal('<>'), pp.Literal('==')

    add = pp.Forward()
    expr = pp.Forward()
    stmt = pp.Forward()
    stmt_list = pp.Forward()

    # Вызов функции - по идее всё как надо
    call = ident + LPAR + pp.Optional(expr + pp.ZeroOrMore(COMMA + expr)) + RPAR

    group = (
            literal |
            call |  # обязательно перед ident, т.к. приоритетный выбор (или использовать оператор ^ вместо | )
            ident |
            LPAR + expr + RPAR
    )

    # обязательно везде pp.Group, иначе приоритет операций не будет работать (см. реализцию set_parse_action_magic);
    # также можно воспользоваться pp.operatorPrecedence (должно быть проще, но не проверял)
    mult = pp.Group(group + pp.ZeroOrMore((MUL | DIV | MOD) + group)).setName('bin_op')
    add << pp.Group(mult + pp.ZeroOrMore((ADD | SUB) + mult)).setName('bin_op')
    compare1 = pp.Group(add + pp.Optional((GE | LE | GT | LT) + add)).setName(
        'bin_op')  # GE и LE первыми, т.к. приоритетный выбор
    compare2 = pp.Group(compare1 + pp.Optional((EQUALS | NEQUALS) + compare1)).setName('bin_op')
    logical_and = pp.Group(compare2 + pp.ZeroOrMore(AND + compare2)).setName('bin_op')
    logical_or = pp.Group(logical_and + pp.ZeroOrMore(OR + logical_and)).setName('bin_op')

    expr << (logical_or)


# Dim i,j,k As Integer
# Dim i = 5
# Dim i As Integer = 5

    simple_assign0 = pp.Optional(AS.suppress() + type_)
    simple_assign1 = pp.Optional(ASSIGN.suppress() + expr)
    var_inner = simple_assign0 | simple_assign1 | ident
    vars_ = DIM.suppress() + var_inner + pp.ZeroOrMore(pp.Optional(COMMA) + var_inner)

# при присваивании значения вне объявления -> i = 5
    assign = ident + ASSIGN.suppress() + expr
    simple_stmt = assign | call  # TODO Возможно не нужен

# 1) For <name> As <type> = <eq1> To <eq2> (Step <eq3>)
#    *Внутренний цикл*
#    Next <name>
# 2) While (условие) (AndAlso (условие2))
# 	*Внутренний цикл*
#    End While
# 3) Do While (условие)
# 	*внутренний цикл*
#    Loop
# 4) If (условие) Then
# ...
# Else
# ...
# End If

    for_cond = expr + TO.suppress() + expr
    vars_for = ident + simple_assign0 + ASSIGN.suppress() + for_cond + pp.Optional(pp.Keyword('Step') + expr)
    #for_stmt_list0 = (pp.Optional(simple_stmt + pp.ZeroOrMore(COMMA + simple_stmt))).setName('stmt_list')
    #for_stmt_list = vars_ | for_stmt_list0
    #for_cond = expr + TO.suppress() + expr
    for_body = stmt | pp.Group(SEMI).setName('stmt_list')

    if_ = IF.suppress() + LPAR + expr + RPAR + stmt + pp.Optional(pp.Keyword("Else").suppress() + stmt) + END.suppress() + IF.suppress()
    while_ = WHILE.suppress() + LPAR + expr + RPAR + stmt + END.suppress() + WHILE.suppress()
    do_ = DO_WHILE.suppress() + LBRACE + expr + RBRACE + stmt + LOOP.suppress()
    for_ = FOR.suppress() + vars_for + for_body + NEXT.suppress() + ident

# Select Case number
#     Case 1 To 5
#         Debug.WriteLine("Between 1 and 5, inclusive")
#         ' The following is the only Case clause that evaluates to True.
#     Case 6, 7, 8
#         Debug.WriteLine("Between 6 and 8, inclusive")
#     Case 9 To 10
#         Debug.WriteLine("Equal to 9 or 10")
#     Case Else
#         Debug.WriteLine("Not between 1 and 10, inclusive")
# End Select

    case_ = CASE.suppress() + pp.oneOf(for_cond, ident + pp.ZeroOrMore(COMMA + ident)) + SEMI + stmt_list
    case_else_ = (CASE_ELSE.suppress() + pp.Group(pp.empty).setName('stmt_list') + stmt_list).setName('case_') # TODO Возможны проблемы с pp.Group
    case_or_default = case_ | case_else_

    select_ = SELECT.suppress() + expr + pp.ZeroOrMore(case_or_default) + END.suppress() + SELECT.suppress()

    return_ = RETURN.suppress() + expr
    composite = LBRACE + stmt_list + RBRACE # TODO непонятно что делать с этим

# Function <name>(<переменная> As <type>) As <typeExit>
# ...
# End Function

    param = ident
    params = pp.Optional(param + pp.ZeroOrMore(COMMA + param))
    params1 = params + pp.Optional(AS.suppress() + type_) + pp.ZeroOrMore(params + AS.suppress() + type_)
    func = FUNC.suppress() + ident + LPAR + params1 + RPAR + AS.suppress() + type_ + stmt_list + END.suppress() + FUNC.suppress()

    stmt << (
            if_ |
            while_ |
            do_ |
            for_ |
            return_ |
            select_ |
            simple_stmt + SEMI |
            # обязательно ниже if, for и т.п., иначе считает их за типы данных (сейчас уже не считает - см. грамматику)
            # обязательно выше vars, иначе посчитает за два vars
            vars_ + SEMI |
            composite |
            func
    )

    stmt_list << (pp.ZeroOrMore(stmt + pp.ZeroOrMore(SEMI)))

    program = stmt_list.ignore(pp.cStyleComment).ignore(pp.dblSlashComment) + pp.StringEnd()

    start = program

# Дальше я не смотрел...

    def set_parse_action_magic(rule_name: str, parser: pp.ParserElement) -> None:
        if rule_name == rule_name.upper():
            return
        if getattr(parser, 'name', None) and parser.name.isidentifier():
            rule_name = parser.name
        if rule_name in ('bin_op',):
            def bin_op_parse_action(s, loc, tocs):
                node = tocs[0]
                if not isinstance(node, AstNode):
                    node = bin_op_parse_action(s, loc, node)
                for i in range(1, len(tocs) - 1, 2):
                    secondNode = tocs[i + 1]
                    if not isinstance(secondNode, AstNode):
                        secondNode = bin_op_parse_action(s, loc, secondNode)
                    node = BinOpNode(BinOp(tocs[i]), node, secondNode, loc=loc)
                return node

            parser.setParseAction(bin_op_parse_action)
        else:
            cls = ''.join(x.capitalize() for x in rule_name.split('_')) + 'Node'
            with suppress(NameError):
                cls = eval(cls)
                if not inspect.isabstract(cls):
                    def parse_action(s, loc, tocs):
                        if cls is FuncNode:
                            return FuncNode(tocs[0], tocs[1], tocs[2:-1], tocs[-1], loc=loc)
                        else:
                            return cls(*tocs, loc=loc)

                    parser.setParseAction(parse_action)

    for var_name, value in locals().copy().items():
        if isinstance(value, pp.ParserElement):
            set_parse_action_magic(var_name, value)

    return start


parser = _make_parser()


def parse(prog: str) -> StmtListNode:
    locs = []
    row, col = 0, 0
    for ch in prog:
        if ch == '\n':
            row += 1
            col = 0
        elif ch == '\r':
            pass
        else:
            col += 1
        locs.append((row, col))

    old_init_action = AstNode.init_action

    def init_action(node: AstNode) -> None:
        loc = getattr(node, 'loc', None)
        if isinstance(loc, int):
            node.row = locs[loc][0] + 1
            node.col = locs[loc][1] + 1

    AstNode.init_action = init_action
    try:
        prog: StmtListNode = parser.parseString(str(prog))[0]
        prog.program = True
        return prog
    finally:
        AstNode.init_action = old_init_action
