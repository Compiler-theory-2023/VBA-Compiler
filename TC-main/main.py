import os
import mel_parser


def main():
    prog = '''
        Function myFunc(a As Integer, b As Boolean) As Integer
            Dim c As Integer = 10
            c = 11
            For s As Integer = 3 To 5
            c = c+1
            Next s
            
            If (a>5) Then
                Dim a  As Integer = 10
            Else 
                Dim a As String = "Just do it!"
            End If  
            
            While (a>5) 
                Dim a  As Integer
            End While                 
            
            Do While (a>5) 
                Dim a  As Integer
            Loop           
        End Function
    
    '''
    prog = mel_parser.parse(prog)
    print(*prog.tree, sep=os.linesep)


if __name__ == "__main__":
    main()
