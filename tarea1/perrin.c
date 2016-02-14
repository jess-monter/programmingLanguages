#include <stdio.h>



int perrin(int n) {
	if(n==0){
		return 3;
	}else if(n==1){
		return 0;
	}else if(n==2){
		return 2;
	}else{
		return perrin(n-3)+perrin(n-2);
	}
}


main() {

	printf("Decimo numero de Perrin \n");
	printf("%d\n", perrin(10));


}

