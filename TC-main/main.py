import os
import mel_parser
import mel_semantic


def main():
    prog = '''    
        Function myFunc(a As Integer, b As String) As String
            Dim c As Integer = 10
            If (a>5) Then
                Dim s As Integer = 3
            Else 
                Dim s As Integer = 3
            End If 
            While (a>5) 
                Dim s As Integer = 3
            End While                 
            
            Do While (a>5) 
                Dim s As Integer = 3
            Loop          
        End Function
        
        Function myFunc2(a As Integer, b As Boolean) As Integer
        End Function

        
    
    '''
    execute(prog)


def execute(prog: str) -> None:
    prog = mel_parser.parse(prog)

    print('ast:')
    print(*prog.tree, sep=os.linesep)
    print()

    print('semantic_check:')
    try:
        scope = mel_semantic.prepare_global_scope()
        prog.semantic_check(scope)
        print(*prog.tree, sep=os.linesep)
    except mel_semantic.SemanticException as e:
        print('Ошибка: {}'.format(e.message))
        return
    print()


if __name__ == "__main__":
    main()
