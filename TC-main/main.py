import os
import mel_parser


def main():
    prog = '''
        Function myFunc(a As Integer, b As Boolean) As Integer
            Dim c As Integer
            
            If (a>5) Then
                Dim a = 14 As Integer
            Else 
                Dim a = 47 As Integer
            End If  
            
            While (a>5) 
                Dim a = 14 As Integer
            End While                 
            
            Do While (a>5) 
                Dim a = 14 As Integer
            Loop
        
           
        End Function
    
    '''
    prog = mel_parser.parse(prog)
    print(*prog.tree, sep=os.linesep)


if __name__ == "__main__":
    main()
