import re
from abc import ABC
from contextlib import suppress
from typing import Callable, Tuple, Optional, Union
from enum import Enum
from mel_semantic import TYPE_CONVERTIBILITY, BIN_OP_TYPE_COMPATIBILITY, BinOp, \
    TypeDesc, IdentDesc, ScopeType, IdentScope, SemanticException


class AstNode(ABC):
    init_action: Callable[['AstNode'], None] = None

    def __init__(self, row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__()
        self.row = row
        self.line = line
        for k, v in props.items():
            setattr(self, k, v)
        if AstNode.init_action is not None:
            AstNode.init_action(self)
        self.node_type: Optional[TypeDesc] = None
        self.node_ident: Optional[IdentDesc] = None

    def __str__(self) -> str:
        pass

    @property
    def childs(self) -> Tuple['AstNode', ...]:
        return ()

    def to_str(self):
        return str(self)

    def to_str_full(self):
        r = ''
        if self.node_ident:
            r = str(self.node_ident)
        elif self.node_type:
            r = str(self.node_type)
        return self.to_str() + (' : ' + r if r else '')

    def semantic_error(self, message: str):
        raise SemanticException(message, self.row, self.line)

    def semantic_check(self, scope: IdentScope) -> None:
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

    def __getitem__(self, index):
        return self.childs[index] if index < len(self.childs) else None


class _GroupNode(AstNode):

    def __init__(self, name: str, *childs: AstNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props) -> None:
        super().__init__(row=row, line=line, **props)
        self.name = name
        self._childs = childs

    def __str__(self) -> str:
        return self.name

    @property
    def childs(self) -> Tuple['AstNode', ...]:
        return self._childs


class ExprNode(AstNode, ABC):
    pass


class VarType(Enum):
    INT = 'int'
    CHAR = 'char'
    STRING = 'string'
    BOOLEAN = 'boolean'
    DOUBLE = 'double'
    VOID = 'void'

class LiteralNode(ExprNode):

    def __init__(self, literal: str,
                 row: Optional[int] = None, line: Optional[int] = None, **props) -> None:
        super().__init__(row=row, line=line, **props)
        self.literal = literal
        if literal in ('true', 'false'):
            self.value = bool(literal)
        else:
            self.value = eval(literal)

    def __str__(self) -> str:
        return self.literal

    def semantic_check(self, scope: IdentScope) -> None:
        if isinstance(self.value, bool):
            self.node_type = TypeDesc.BOOL
        # проверка должна быть позже bool, т.к. bool наследник от int
        elif isinstance(self.value, int):
            self.node_type = TypeDesc.INT
        elif isinstance(self.value, float):
            self.node_type = TypeDesc.DOUBLE
        elif isinstance(self.value, str) and re.fullmatch(r'\'\w\'', self.value):
            self.node_type = TypeDesc.CHAR
        elif isinstance(self.value, str):
            self.node_type = TypeDesc.STR
        else:
            self.semantic_error('Неизвестный тип {} для {}'.format(type(self.value), self.value))


class IdentNode(ExprNode):
    def __init__(self, name: str,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.name = str(name)

    def __str__(self) -> str:
        return str(self.name)

    def semantic_check(self, scope: IdentScope) -> None:
        ident = scope.get_ident(self.name)
        if ident is None:
            self.semantic_error('Идентификатор {} не найден'.format(self.name))
        self.node_type = ident.type
        self.node_ident = ident


class TypeNode(IdentNode):
    """Класс для представления в AST-дереве типов данный
           (при появлении составных типов данных должен быть расширен)
    """

    def __init__(self, name: str,
                 row: Optional[int] = None, line: Optional[int] = None, **props) -> None:
        super().__init__(name, row=row, line=line, **props)
        self.type = None
        with suppress(SemanticException):
            self.type = TypeDesc.from_str(name)

    def to_str_full(self):
        return self.to_str()

    def semantic_check(self, scope: IdentScope) -> None:
        if self.type is None:
            self.semantic_error('Неизвестный тип {}'.format(self.name))


class TypeConvertNode(ExprNode):
    """Класс для представления в AST-дереве операций конвертации типов данных
       (в языке программирования может быть как expression, так и statement)
    """

    def __init__(self, expr: ExprNode, type_: TypeDesc,
                 row: Optional[int] = None, col: Optional[int] = None, **props) -> None:
        super().__init__(row=row, col=col, **props)
        self.expr = expr
        self.type = type_
        self.node_type = type_

    def __str__(self) -> str:
        return 'convert'

    @property
    def childs(self) -> Tuple[AstNode, ...]:
        return (_GroupNode(str(self.type), self.expr),)


def type_convert(expr: ExprNode, type_: TypeDesc, except_node: Optional[AstNode] = None,
                 comment: Optional[str] = None) -> ExprNode:
    """Метод преобразования ExprNode узла AST-дерева к другому типу
    :param expr: узел AST-дерева
    :param type_: требуемый тип
    :param except_node: узел, о которого будет исключение
    :param comment: комментарий
    :return: узел AST-дерева c операцией преобразования
    """

    if expr.node_type is None:
        except_node.semantic_error('Тип выражения не определен')
    if expr.node_type == type_:
        return expr
    if expr.node_type.is_simple and type_.is_simple and \
            expr.node_type.base_type in TYPE_CONVERTIBILITY and type_.base_type in TYPE_CONVERTIBILITY[expr.node_type.base_type]:
        return TypeConvertNode(expr, type_)
    else:
        (except_node if except_node else expr).semantic_error('Тип {0}{2} не конвертируется в {1}'.format(
            expr.node_type, type_, ' ({})'.format(comment) if comment else ''
        ))


class StmtNode(ExprNode, ABC):
    """Абстракный класс для деклараций или инструкций в AST-дереве
    """

    def to_str_full(self):
        return self.to_str()


class AssignNode(StmtNode):
    """Класс для представления в AST-дереве оператора присваивания
    """

    def __init__(self, ident: IdentNode, var: VarType, val: ExprNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props) -> None:
        super().__init__(row=row, Line=line, **props)
        self.ident = ident
        self.var = var
        self.val = val

    def __str__(self) -> str:
        return '='

    @property
    def childs(self) -> Tuple[IdentNode, VarType, ExprNode]:
        return self.ident, self.var, self.val

    def semantic_check(self, scope: IdentScope) -> None:
        self.var.semantic_check(scope)
        self.val.semantic_check(scope)
        self.val = type_convert(self.val, self.var.node_type, self, 'присваиваемое значение')
        self.node_type = self.var.node_type


class VarsNode(StmtNode):
    """Класс для представления в AST-дереве объявления переменнных
    """

    def __init__(self, type_: TypeNode, *vars_: Union[IdentNode, 'AssignNode'],
                 row: Optional[int] = None, line: Optional[int] = None, **props) -> None:
        super().__init__(row=row, line=line, **props)
        self.type = type_
        self.vars = vars_

    def __str__(self) -> str:
        return str(self.type)

    @property
    def childs(self) -> Tuple[AstNode, ...]:
        return self.vars

    def semantic_check(self, scope: IdentScope) -> None:
        self.type.semantic_check(scope)
        for var in self.vars:
            var_node: IdentNode = var.var if isinstance(var, AssignNode) else var
            try:
                scope.add_ident(IdentDesc(var_node.name, self.type.type))
            except SemanticException as e:
                var_node.semantic_error(e.message)
            var.semantic_check(scope)
        self.node_type = TypeDesc.VOID


class ReturnNode(StmtNode):
    """Класс для представления в AST-дереве оператора return
    """

    def __init__(self, val: ExprNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props) -> None:
        super().__init__(row=row, line=line, **props)
        self.val = val

    def __str__(self) -> str:
        return 'Return'

    @property
    def childs(self) -> Tuple[ExprNode]:
        return (self.val, )

    def semantic_check(self, scope: IdentScope) -> None:
        self.val.semantic_check(IdentScope(scope))
        func = scope.curr_func
        if func is None:
            self.semantic_error('Оператор return применим только к функции')
        self.val = type_convert(self.val, func.func.type.return_type, self, 'возвращаемое значение')
        self.node_type = TypeDesc.VOID


class VarType(Enum):
    INT = 'Integer'
    CHAR = 'Char'
    STRING = 'String'
    BOOLEAN = 'Boolean'
    DOUBLE = 'Double'


class FuncVarNode(ExprNode):
    def __init__(self, type_: TypeNode, ident: IdentNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.type = type_
        self.ident = ident

    @property
    def childs(self) -> tuple[TypeNode, IdentNode]:
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
    def __init__(self, type_: TypeNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.type = type_

    def __str__(self) -> str:
        return str(self.type)


class ArrayTypeNode(ExprNode):
    def __init__(self, type_: TypeNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.type = type_

    def __str__(self) -> str:
        return 'array {0}'.format(str(self.type))


#TODO What is this?
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


# TODO What is this?
class VarsDeclNode(StmtNode):
    def __init__(self, vars_type: TypeNode, *vars_list: Tuple[AstNode, ...],
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.vars_type = vars_type
        self.vars_list = vars_list

    @property
    def childs(self) -> Tuple[ExprNode, ...]:
        return (self.vars_type,) + self.vars_list

    def __str__(self) -> str:
        return 'var'


#TODO What is this?
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
                 row: Optional[int] = None, line: Optional[int] = None, **props) -> None:
        super().__init__(row=row, line=line, **props)
        self.func = func
        self.params = params

    def __str__(self) -> str:
        return 'call'

    @property
    def childs(self) -> Tuple[IdentNode, ...]:
        return (self.func,) + self.params

    def semantic_check(self, scope: IdentScope) -> None:
        func = scope.get_ident(self.func.name)
        if func is None:
            self.semantic_error('Функция {} не найдена'.format(self.func.name))
        if not func.type.func:
            self.semantic_error('Идентификатор {} не является функцией'.format(func.name))
        if len(func.type.params) != len(self.params):
            self.semantic_error('Кол-во аргументов {} не совпадает (ожидалось {}, передано {})'.format(
                func.name, len(func.type.params), len(self.params)
            ))
        params = []
        error = False
        decl_params_str = fact_params_str = ''
        for i in range(len(self.params)):
            param: ExprNode = self.params[i]
            param.semantic_check(scope)
            if len(decl_params_str) > 0:
                decl_params_str += ', '
            decl_params_str += str(func.type.params[i])
            if len(fact_params_str) > 0:
                fact_params_str += ', '
            fact_params_str += str(param.node_type)
            try:
                params.append(type_convert(param, func.type.params[i]))
            except:
                error = True
        if error:
            self.semantic_error('Фактические типы ({1}) аргументов функции {0} не совпадают с формальными ({2})\
                                    и не приводимы'.format(
                func.name, fact_params_str, decl_params_str
            ))
        else:
            self.params = tuple(params)
            self.func.node_type = func.type
            self.func.node_ident = func
            self.node_type = func.type.return_type


# TODO What is this?
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
        return 'If'

    def semantic_check(self, scope: IdentScope) -> None:
        self.cond.semantic_check(scope)
        self.cond = type_convert(self.cond, TypeDesc.BOOL, None, 'условие')
        self.then_stmt.semantic_check(IdentScope(scope))
        if self.else_stmt:
            self.else_stmt.semantic_check(IdentScope(scope))
        self.node_type = TypeDesc.VOID


class ForNode(StmtNode):
    def __init__(self, cond: StmtNode, expr: ExprNode, then_stmt: StmtNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.cond = cond
        self.expr = expr
        self.then_stmt = then_stmt

    @property
    def childs(self) -> tuple[StmtNode, ExprNode, StmtNode]:
        return self.cond, self.expr, self.then_stmt

    def __str__(self) -> str:
        return 'For'

    def childs(self) -> Tuple[AstNode, ...]:
        return self.init, self.cond, self.step, self.body

    def semantic_check(self, scope: IdentScope) -> None:
        scope = IdentScope(scope)
        self.init.semantic_check(scope)
        if self.cond == _empty:
            self.cond = LiteralNode('true')
        self.cond.semantic_check(scope)
        self.cond = type_convert(self.cond, TypeDesc.BOOL, None, 'условие')
        self.step.semantic_check(scope)
        self.body.semantic_check(IdentScope(scope))
        self.node_type = TypeDesc.VOID


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
        return 'While'


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
        return 'Do-While'


class StmtListNode(StmtNode):
    def __init__(self, *exprs: StmtNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.exprs = exprs

    @property
    def childs(self) -> Tuple[StmtNode, ...]:
        return self.exprs

    def __str__(self) -> str:
        return ""


class ParamNode(StmtNode):
    """Класс для представления в AST-дереве объявления параметра функции
    """

    def __init__(self, type_: TypeNode, name: IdentNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props) -> None:
        super().__init__(row=row, line=line, **props)
        self.type = type_
        self.name = name

    def __str__(self) -> str:
        return str(self.type)

    @property
    def childs(self) -> Tuple[IdentNode]:
        return self.name,

    def semantic_check(self, scope: IdentScope) -> None:
        self.type.semantic_check(scope)
        self.name.node_type = self.type.type
        try:
            self.name.node_ident = scope.add_ident(IdentDesc(self.name.name, self.type.type, ScopeType.PARAM))
        except SemanticException:
            raise self.name.semantic_error('Параметр {} уже объявлен'.format(self.name.name))
        self.node_type = TypeDesc.VOID


class FuncNode(StmtNode):
    def __init__(self, name: IdentNode, func_vars: ParamNode, return_type: TypeNode,
                 stmt_list: StmtListNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.return_type = return_type
        self.name = name
        self.func_vars = func_vars
        self.stmt_list = stmt_list

    def __str__(self) -> str:
        return 'Function'
    @property
    def childs(self) -> Tuple[Optional[FuncVarsListNode], StmtListNode]:
        return ((self.func_vars,) if self.func_vars else tuple()) + (self.stmt_list,)

    def __str__(self) -> str:
        return '({0}) func {1}'.format(self.return_type, self.name)

    def semantic_check(self, scope: IdentScope) -> None:
        if scope.curr_func:
            self.semantic_error(
                "Объявление функции ({}) внутри другой функции не поддерживается".format(self.name.name))
        parent_scope = scope
        self.return_type.semantic_check(scope)
        scope = IdentScope(scope)

        # временно хоть какое-то значение, чтобы при добавлении параметров находить scope функции
        scope.func = _empty_ident
        params = []
        for param in self.params:
            # при проверке параметров происходит их добавление в scope
            param.semantic_check(scope)
            params.append(param.type.type)

        type_ = TypeDesc(None, self.type.type, tuple(params))
        func_ident = IdentDesc(self.name.name, type_)
        scope.func = func_ident
        self.name.node_type = type_
        try:
            self.name.node_ident = parent_scope.curr_global.add_ident(func_ident)
        except SemanticException as e:
            self.name.semantic_error("Повторное объявление функции {}".format(self.name.name))
        self.body.semantic_check(scope)
        self.node_type = TypeDesc.VOID


class ProgNode(StmtNode):
    def __init__(self, *funcs_and_vars: StmtNode, row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.funcs_and_vars = funcs_and_vars

    @property
    def childs(self) -> Tuple[StmtNode, ...]:
        return self.funcs_and_vars

    def __str__(self) -> str:
        return 'prog'


_empty = StmtListNode()
_empty_ident = IdentDesc('', TypeDesc.VOID)
