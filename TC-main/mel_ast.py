from abc import ABC, abstractmethod
from typing import Callable, Tuple, Optional, Union
from enum import Enum


class AstNode(ABC):
    def __init__(self, row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__()
        self.row = row
        self.line = line
        for k, v in props.items():
            setattr(self, k, v)

    @property
    def childs(self) -> Tuple['AstNode', ...]:
        return ()

    @abstractmethod
    def __str__(self) -> str:
        pass

    @property
    def tree(self) -> [str, ...]:
        res = [str(self)]
        childs_temp = self.childs
        for i, child in enumerate(childs_temp):
            ch0, ch = '├', '│'
            if i == len(childs_temp) - 1:
                ch0, ch = '└', ' '
            res.extend(((ch0 if j == 0 else ch) + ' ' + s for j, s in enumerate(child.tree)))
        return res

    def visit(self, func: Callable[['AstNode'], None]) -> None:
        func(self)
        map(func, self.childs)

    def __getitem__(self, index):
        return self.childs[index] if index < len(self.childs) else None


class ExprNode(AstNode):
    pass


class ExprListNode(ExprNode):
    def __init__(self, *exprs: ExprNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.exprs = exprs

    @property
    def childs(self) -> Tuple[ExprNode, ...]:
        return self.exprs

    def __str__(self) -> str:
        return '{}'


class LiteralNode(ExprNode):
    def __init__(self, literal: str,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.literal = literal
        self.value = eval(literal.replace('\'', '\"'))

    def __str__(self) -> str:
        return '{0}'.format(self.literal, type(self.value).__name__)


class IdentNode(ExprNode):
    def __init__(self, name: str,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.name = str(name)

    def __str__(self) -> str:
        return str(self.name)


class VarType(Enum):
    INT = 'int'
    CHAR = 'char'
    STRING = 'string'
    BOOLEAN = 'boolean'
    DOUBLE = 'double'
    VOID = 'void'


class FuncVarNode(ExprNode):
    def __init__(self, type: VarType, ident: IdentNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.type = type
        self.ident = ident

    @property
    def childs(self) -> Tuple[VarType, IdentNode]:
        return self.type, self.ident

    def __str__(self) -> str:
        return 'var-sign'


class FuncVarsListNode(ExprNode):
    def __init__(self, *vars: FuncVarNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.vars = vars

    @property
    def childs(self) -> Tuple[FuncVarNode, ...]:
        return self.vars

    def __str__(self) -> str:
        return 'func-vars'


class SimpleTypeNode(ExprNode):
    def __init__(self, type: VarType,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.type = type

    def __str__(self) -> str:
        return str(self.type)


class ArrayTypeNode(ExprNode):
    def __init__(self, type: VarType,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.type = type

    def __str__(self) -> str:
        return 'array {0}'.format(str(self.type))


class TypeNode(ExprNode):
    def __init__(self, type: ExprNode, row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.type = type

    def __str__(self) -> str:
        return str(self.type)


class TypeListNode(ExprNode):
    def __init__(self, *types: TypeNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.types = types

    @property
    def childs(self) -> Tuple[TypeNode, ...]:
        return self.types

    def __str__(self) -> str:
        return 'type-list'


class FuncReturnTypeNode(TypeNode):
    pass


class BinOp(Enum):
    ADD = '+'
    SUB = '-'
    MUL = '*'
    DIV = '/'
    MOD = '%'
    GE = '>='
    LE = '<='
    NEQUALS = '<>'
    EQUALS = '=='
    GT = '>'
    LT = '<'
    BIT_AND = '&'
    BIT_OR = '|'
    LOGICAL_AND = '&&'
    LOGICAL_OR = '||'


class BinOpNode(ExprNode):
    def __init__(self, op: BinOp, arg1: ExprNode, arg2: ExprNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.op = op
        self.arg1 = arg1
        self.arg2 = arg2

    @property
    def childs(self) -> Tuple[ExprNode, ExprNode]:
        return self.arg1, self.arg2

    def __str__(self) -> str:
        return str(self.op.value)


class StmtNode(ExprNode):
    pass


class VarsDeclNode(StmtNode):
    def __init__(self, vars_type: TypeNode, *vars_list: Tuple[AstNode, ...],
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.vars_type = vars_type
        self.vars_list = vars_list

    @property
    def childs(self) -> Tuple[ExprNode, ...]:
        # return self.vars_type, (*self.vars_list)
        return (self.vars_type,) + self.vars_list

    def __str__(self) -> str:
        return 'var'


class VarsDeclListNode(StmtNode):
    def __init__(self, *vars_decl: VarsDeclNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.vars_decl = vars_decl

    @property
    def childs(self) -> Tuple[VarsDeclNode, ...]:
        return self.vars_decl

    def __str__(self) -> str:
        return 'vars_list'


class CallNode(StmtNode):
    def __init__(self, func: IdentNode, *params: Tuple[ExprNode],
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.func = func
        self.params = params

    @property
    def childs(self) -> Tuple[IdentNode, ...]:
        # return self.func, (*self.params)
        return (self.func,) + self.params

    def __str__(self) -> str:
        return 'call'


class AssignNode(StmtNode):
    def __init__(self, var: IdentNode, val: ExprNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.var = var
        self.val = val

    @property
    def childs(self) -> Tuple[IdentNode, ExprNode]:
        return self.var, self.val

    def __str__(self) -> str:
        return '='


class ComplexIdentNode(StmtNode):
    def __init__(self, var: IdentNode, index: ExprNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.var = var
        self.index = index

    @property
    def childs(self) -> Tuple[IdentNode, ExprNode]:
        return self.var, self.index

    def __str__(self) -> str:
        return 'array_elem'


class IfNode(StmtNode):
    def __init__(self, cond: ExprNode, then_stmt: StmtNode, else_stmt: Optional[StmtNode] = None,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.cond = cond
        self.then_stmt = then_stmt
        self.else_stmt = else_stmt

    @property
    def childs(self) -> Tuple[ExprNode, StmtNode, Optional[StmtNode]]:
        return (self.cond, self.then_stmt) + ((self.else_stmt,) if self.else_stmt else tuple())

    def __str__(self) -> str:
        return 'if'


class ForNode(StmtNode):
    def __init__(self, init: Union[StmtNode, None], cond: Union[ExprNode, StmtNode, None],
                 step: Union[StmtNode, None], body: Union[StmtNode, None] = None,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.init = init if init else _empty
        self.cond = cond if cond else _empty
        self.step = step if step else _empty
        self.body = body if body else _empty

    @property
    def childs(self) -> Tuple[AstNode, ...]:
        return self.init, self.cond, self.step, self.body

    def __str__(self) -> str:
        return 'for'


class WhileNode(StmtNode):
    def __init__(self, cond: ExprNode, stmt: StmtNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.cond = cond
        self.stmt = stmt

    @property
    def childs(self) -> Tuple[ExprNode, StmtNode]:
        return self.cond, self.stmt

    def __str__(self) -> str:
        return 'while'


class DoWhileNode(StmtNode):
    def __init__(self, stmt: StmtNode, cond: ExprNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.stmt = stmt
        self.cond = cond

    @property
    def childs(self) -> Tuple[ExprNode, StmtNode]:
        return self.cond, self.stmt

    def __str__(self) -> str:
        return 'do-while'


class StmtListNode(StmtNode):
    def __init__(self, *exprs: StmtNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.exprs = exprs

    @property
    def childs(self) -> Tuple[StmtNode, ...]:
        return self.exprs

    def __str__(self) -> str:
        return '...'


class FuncNode(StmtNode):
    def __init__(self, return_type: FuncReturnTypeNode, name: IdentNode, func_vars: FuncVarsListNode,
                 stmt_list: StmtListNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.return_type = return_type
        self.name = name
        self.func_vars = func_vars
        self.stmt_list = stmt_list

    @property
    def childs(self) -> Tuple[Optional[FuncVarsListNode], StmtListNode]:
        return ((self.func_vars,) if self.func_vars else tuple()) + (self.stmt_list,)

    def __str__(self) -> str:
        return '({0}) func {1}'.format(self.return_type, self.name)


class ProgNode(StmtNode):
    def __init__(self, *funcs_and_vars: StmtNode, row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.funcs_and_vars = funcs_and_vars

    @property
    def childs(self) -> Tuple[StmtNode, ...]:
        return self.funcs_and_vars

    def __str__(self) -> str:
        return 'prog'


class DelegateNode(StmtNode):
    def __init__(self, type_list: TypeListNode, return_type: FuncReturnTypeNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.type_list = type_list
        self.return_type = return_type

    @property
    def childs(self) -> Tuple[TypeListNode, FuncReturnTypeNode]:
        return self.type_list, self.return_type

    def __str__(self) -> str:
        return 'delegate'


_empty = StmtListNode()
