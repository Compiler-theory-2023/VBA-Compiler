from lark import Lark
from lark.lexer import Token
from lark.visitors import InlineTransformer

from mel_ast import *

parser = Lark('''
    %import common.NUMBER
    %import common.ESCAPED_STRING
    %import common.CNAME
    %import common.NEWLINE
    %import common.WS

    %ignore WS

    COMMENT: "/*" /(.|\\n|\\r)+/ "*/"
        |  "//" /(.)+/ NEWLINE
    %ignore COMMENT
    
    INTEGER:    "Integer"
    CHAR:       "Char"
    STRING:     "String"
    BOOLEAN:    "Boolean"
    DOUBLE:     "Double"
    
    TRUE:       "True"
    FALSE:      "False"
    ESCAPED_CHAR: "\'" /./ "\'"
    
    IF: "If"
    THEN: "Then"
    ELSE: "Else"
    END: "End"
    DO: "Do"
    WHILE: "While"
    LOOP: "Loop"
    FOR: "For"
    AS: "As"
    TO: "To"
    NEXT: "Next"
    DIM: "Dim"
    
    num: NUMBER  -> literal
    char: ESCAPED_CHAR  -> literal
    str: ESCAPED_STRING  -> literal
    bool: (TRUE|FALSE)  -> literal
    type: INTEGER | STRING | CHAR | BOOLEAN | DOUBLE
    ident: CNAME
    key: IF | THEN | ELSE | END | DO | WHILE | LOOP | FOR | AS | TO | NEXT | DIM -> key

    ADD:     "+"
    SUB:     "-"
    MUL:     "*"
    DIV:     "/"
    MOD:     "Mod"
    AND:     "AndAlso"
    OR:      "OrElse"
    BIT_AND: "And"
    BIT_OR:  "Or"
    GE:      ">="
    LE:      "<="
    NEQUALS: "<>"
    EQUALS:  "=="
    GT:      ">"
    LT:      "<"

    call: ident  ( expr ( "," expr )* )? 
    
    type_list: (type ("," type)*)?
    
    ?delegate: "delegate" "<" type_list ":" "As" type ">"
    
    ?group: num | char| str | bool
        | ident
        | key
        | "(" expr ")"

    ?mult: group
        | mult ( MUL | DIV |  MOD) group  -> bin_op

    ?add: mult
        | add ( ADD | SUB ) mult  -> bin_op

    ?compare1: add
        | add ( GT | LT | GE | LE ) add  -> bin_op

    ?compare2: compare1
        | compare1 ( EQUALS | NEQUALS ) compare1  -> bin_op

    ?logical_and: compare2
        | logical_and AND compare2  -> bin_op

    ?logical_or: logical_and
        | logical_or OR logical_and  -> bin_op

    ?expr: logical_or
    
    ?expr_list: "{" expr ( "," expr )* "}"
        
    ?assign: ident "As" type ("=" expr)?-> vars_assign
    
    ?vars_assign: ident "=" expr -> assign

    ?for_stmt_list: ( assign ( "," assign )* )?  -> stmt_list
    ?for_cond: expr
        |   -> stmt_list
    ?for_body: stmt
    
    ?return: "Return" expr -> return
        
    ?stmt: func
        | "If" "(" expr ")" "Then" stmt ("Else" stmt)? "End If" -> if
        | "For" assign "To" expr  stmt "Next" -> for
        | "Do" "While" "(" expr ")" stmt "Loop"-> do_while
        | "While" "(" expr ")" stmt "End While"-> while
        | return
        | "Dim" assign
        | vars_assign
      
    ?stmt_list: ( stmt )*
    
    ?func_var: ident "As" type -> func_var
    ?func_vars_list: (func_var ("," func_var)*)?
    
    func: "Function" ident "(" func_vars_list ")" "As" type stmt_list "End Function" 
    
    ?prog: (stmt_list | func)*

    ?start: prog
''', start='start')  # , parser='lalr')


class MelASTBuilder(InlineTransformer):
    def __getattr__(self, item):
        if isinstance(item, str) and item.upper() == item:
            return lambda x: x

        if item in ('bin_op',):
            def get_bin_op_node(*args):
                op = BinOp(args[1].value)
                return BinOpNode(op, args[0], args[2],
                                 **{'token': args[1], 'line': args[1].line, 'column': args[1].column})

            return get_bin_op_node
        else:
            def get_node(*args):
                props = {}
                if len(args) == 1 and isinstance(args[0], Token):
                    props['token'] = args[0]
                    props['line'] = args[0].line
                    props['column'] = args[0].column
                    args = [args[0].value]
                cls = eval(''.join(x.capitalize() for x in item.split('_')) + 'Node')
                return cls(*args, **props)

            return get_node


def parse(prog: str) -> ProgNode:
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
        prog: ProgNode = parser.parse(str(prog))
        prog.program = True
        prog = MelASTBuilder().transform(prog)
        return prog
    finally:
        AstNode.init_action = old_init_action



