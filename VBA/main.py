import os
import sys

from compiler_demo import program


def main() -> None:

    prog6 = '''
    int main() {
    int pass;
    printf("Введите пароль: ");
    scanf( pass);
    if (pass == 1234) { // пароль 1234
            printf("Вход разрешён");
    } else {
        printf("Вы ввели неверный пароль");
    }
}
    '''
    prog71 = '''
        int main() {
  int i=0;
 while(i<10){
 i=i+1;
 }

}
    '''

    prog7='''
   int main() {
  int i=0;
  do {
  print(i);
  } while(i<10);
}
        '''
    prog8 = '''
      float sqr(float x) {
    return x * x;
}
int main() {
int t=sqr(4.2);
    printf("Квадрат числа 4,2 равен ", t);

}
            '''
    prog9 = '''
     int main(h a, h b) {
     int day;
 
    printf("Введите номер дня недели");
    scanf("%d", day);
   if (day > 7) {
        day = day % 7;
    }
 switch (day + t) {
 /*
 case 1
        printf("Понедельник");
        return day;
 case 2
        printf("Вторник");
        return day;
 case 3
        printf("Среда");
        return day;
 case 4
        printf("Четверг");
        return day;
 case 5
        printf("Пятница");
        return day;
*/
 case 6+y:
        printf("Суббота");
        return day;    
 default :
      printf("Воскресенье");
        return day;    
 }
}

                '''

    program.execute(prog9)


if __name__ == "__main__":
    main()
