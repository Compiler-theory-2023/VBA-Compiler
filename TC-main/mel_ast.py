import re
from abc import ABC
from contextlib import suppress
from typing import Callable, Tuple, Optional
from mel_semantic import TYPE_CONVERTIBILITY, BIN_OP_TYPE_COMPATIBILITY, BinOp, \
    TypeDesc, IdentDesc, ScopeType, IdentScope, SemanticException, BaseType, KeyWord
from msil import CodeLabel, CodeGenerator


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
        res = [str(self.to_str_full())]
        childs_temp = self.childs
        for i, child in enumerate(childs_temp):
            ch0, ch = '├', '│'
            if i == len(childs_temp) - 1:
                ch0, ch = '└', ' '
            res.extend(((ch0 if j == 0 else ch) + ' ' + s for j, s in enumerate(child.tree)))
        return tuple(res)

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


class LiteralNode(ExprNode):

    def __init__(self, literal: str,
                 row: Optional[int] = None, line: Optional[int] = None, **props) -> None:
        super().__init__(row=row, line=line, **props)
        self.literal = literal
        if literal in ('True', 'False'):
            self.value = bool(literal)
        else:
            self.value = eval(literal)

    def __str__(self) -> str:
        return self.literal

    def semantic_check(self, scope: IdentScope) -> None:
        if isinstance(self.value, bool):
            self.node_type = TypeDesc.BOOL
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


class KeyNode(ExprNode):
    def __init__(self, key: KeyWord,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.key = key

    def __str__(self) -> str:
        return str(self.key)


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

    def semantic_check(self, scope: IdentScope) -> None:
        self.arg1.semantic_check(scope)
        self.arg2.semantic_check(scope)

        if self.arg1.node_type.is_simple or self.arg2.node_type.is_simple:
            compatibility = BIN_OP_TYPE_COMPATIBILITY[self.op]
            args_types = (self.arg1.node_type.base_type, self.arg2.node_type.base_type)
            if args_types in compatibility:
                self.node_type = TypeDesc.from_base_type(compatibility[args_types])
                return

            if self.arg2.node_type.base_type in TYPE_CONVERTIBILITY:
                for arg2_type in TYPE_CONVERTIBILITY[self.arg2.node_type.base_type]:
                    args_types = (self.arg1.node_type.base_type, arg2_type)
                    if args_types in compatibility:
                        self.arg2 = type_convert(self.arg2, TypeDesc.from_base_type(arg2_type))
                        self.node_type = TypeDesc.from_base_type(compatibility[args_types])
                        return
            if self.arg1.node_type.base_type in TYPE_CONVERTIBILITY:
                for arg1_type in TYPE_CONVERTIBILITY[self.arg1.node_type.base_type]:
                    args_types = (arg1_type, self.arg2.node_type.base_type)
                    if args_types in compatibility:
                        self.arg1 = type_convert(self.arg1, TypeDesc.from_base_type(arg1_type))
                        self.node_type = TypeDesc.from_base_type(compatibility[args_types])
                        return

        self.semantic_error("Оператор {} не применим к типам ({}, {})".format(
            self.op, self.arg1.node_type, self.arg2.node_type
        ))


class CallNode(ExprNode):
    def __init__(self, func: IdentNode, *params: Tuple[ExprNode], #TODO ???
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


class TypeConvertNode(ExprNode):

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

    def to_str_full(self):
        return self.to_str()


class VarsAssignNode(StmtNode):

    def __init__(self, ident: IdentNode, var: TypeNode, val: Optional[ExprNode] = None,
                 row: Optional[int] = None, line: Optional[int] = None, **props) -> None:
        super().__init__(row=row, Line=line, **props)
        self.ident = ident
        self.var = var
        self.val = val

    def __str__(self) -> str:
        return '='

    @property
    def childs(self) -> Tuple[IdentNode, TypeNode, Optional[ExprNode]]:
        return self.ident, self.var, *((self.val,) if self.val else tuple())

    def semantic_check(self, scope: IdentScope) -> None:
        self.var.semantic_check(scope)
        self.ident.semantic_check(scope)
        self.node_type = TypeDesc.VOID


class AssignNode(ExprNode):
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

    def semantic_check(self, scope: IdentScope) -> None:
        self.var.semantic_check(scope)
        self.val.semantic_check(scope)
        self.val = type_convert(self.val, self.var.node_type, self, 'присваиваемое значение')
        self.node_type = self.var.node_type


class ReturnNode(StmtNode):

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

    def semantic_check(self, scope: IdentScope) -> None:
        self.type.semantic_check(scope)
        self.ident.node_type = self.type.type
        try:
            self.ident.node_ident = scope.add_ident(IdentDesc(self.ident.name, self.type.type, ScopeType.PARAM))
        except SemanticException:
            raise self.ident.semantic_error('Параметр {} уже объявлен'.format(self.ident.name))
        self.node_type = TypeDesc.VOID


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

    def semantic_check(self, scope: IdentScope) -> None:
        scope.func = _empty_ident
        params = []
        for param in self.vars:
            param.semantic_check(scope)
            params.append(param.type.type)


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

    def msil(self, gen: CodeGenerator) -> None:
        self.cond.msil(gen)
        gen.add('    ldc.i4', 0)
        gen.add('    ceq')
        else_label = CodeLabel()
        end_label = CodeLabel()
        gen.add('    brtrue', else_label)
        self.then_stmt.msil(gen)
        gen.add('    br', end_label)
        gen.add('', label=else_label)
        if self.else_stmt:
            self.else_stmt.msil(gen)
        gen.add('', label=end_label)


class ForNode(StmtNode):
    def __init__(self, cond: StmtNode, expr: ExprNode, stmt: StmtNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.cond = cond
        self.expr = expr
        self.stmt = stmt

    @property
    def childs(self) -> tuple[StmtNode, ExprNode, StmtNode]:
        return self.cond, self.expr, self.stmt

    def __str__(self) -> str:
        return 'For'

    def semantic_check(self, scope: IdentScope) -> None:
        if self.cond == _empty:
            self.cond = LiteralNode('True')
        self.cond.semantic_check(scope)
        self.cond = type_convert(self.cond, TypeDesc.BOOL, None, 'условие')
        self.stmt.semantic_check(IdentScope(scope))
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
                 row: Optional[int] = None, col: Optional[int] = None, **props) -> None:
        super().__init__(row=row, col=col, **props)
        self.exprs = exprs
        self.program = False

    def __str__(self) -> str:
        return '...'

    @property
    def childs(self) -> Tuple[StmtNode, ...]:
        return self.exprs

    def semantic_check(self, scope: IdentScope) -> None:
        if not self.program:
            scope = IdentScope(scope)
        for expr in self.exprs:
            expr.semantic_check(scope)
        self.node_type = TypeDesc.VOID

    def msil(self, gen: CodeGenerator) -> None:
        for expr in self.exprs:
            expr.msil(gen)


class FuncNode(StmtNode):
    def __init__(self, name: IdentNode, func_vars: FuncVarsListNode, return_type: TypeNode,
                 stmt_list: StmtNode,
                 row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.return_type = return_type
        self.name = name
        self.func_vars = func_vars
        self.stmt_list = stmt_list

    def __str__(self) -> str:
        return 'Function'

    @property
    def childs(self) -> Tuple[AstNode, ...]:
        return _GroupNode(str(self.return_type), self.name), self.func_vars, self.stmt_list

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
        for var in self.func_vars.vars:
            # при проверке параметров происходит их добавление в scope
            # var.semantic_check(scope)
            # params.append(var.type.type)
            scope.add_ident(IdentDesc(var.type.name, var.ident.type))

        type_ = TypeDesc(None, self.return_type.type, tuple(params))
        func_ident = IdentDesc(self.name.name, type_)
        scope.func = func_ident
        self.name.node_type = type_
        try:
            self.name.node_ident = parent_scope.curr_global.add_ident(func_ident)
        except SemanticException as e:
            self.name.semantic_error("Повторное объявление функции {}".format(self.name.name))
        self.stmt_list.semantic_check(scope)
        self.node_type = TypeDesc.VOID

    def msil(self, gen: CodeGenerator) -> None:
        params = ''
        for p in self.func_vars:
            if len(params) > 0:
                params += ', '
            params += 'int32 ' + str(p.name.name)
        gen.add('  .method public static void {}({}) cil managed'.format(self.name, params))
        '''
        if 
        line = '.local init'

        .locals
        init([0]
        int32
        c,
        [1]
        int32
        CS$1$0000)
        '''
        self.stmt_list.msil(gen)


class ProgNode(StmtNode):
    def __init__(self, *funcs_and_vars: StmtListNode, row: Optional[int] = None, line: Optional[int] = None, **props):
        super().__init__(row=row, line=line, **props)
        self.funcs_and_vars = funcs_and_vars

    @property
    def childs(self) -> Tuple[StmtNode, ...]:
        return self.funcs_and_vars

    def __str__(self) -> str:
        return 'prog'


_empty = StmtListNode()
_empty_ident = IdentDesc('', TypeDesc.VOID)
