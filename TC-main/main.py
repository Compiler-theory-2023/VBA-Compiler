import os
import mel_parser


def main():
    prog = '''
        void myFunc(int[] a, boolean b) {
        
            if (a>5) then
                Dim a = 14 As int
            else 
                Dim a = 47 As int
            end if  
            
            while (a>5) 
                Dim a = 14 As int
            end while                 
            
            do while (a>5) 
                Dim a = 14 As int
            Loop
        
                    
            for a As int = 1 To 2
            Dim a = 14 As int
            Next
        }
    
    '''
    prog = mel_parser.parse(prog)
    print(*prog.tree, sep=os.linesep)


if __name__ == "__main__":
    main()
