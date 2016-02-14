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

int perrin2(int n) {
	int pr;
	if(n==0){
		pr = 3;
	}else if(n==1){
		pr = 0;
	}else if(n==2){
		pr = 2;
	}else{
		pr = perrin(n-3)+perrin(n-2);
	}
	return pr;
}

int perrin3(int n) {
	int perrinArr[n+1];
	int a;
	perrinArr[0] = 3;
	perrinArr[1] = 0;
	perrinArr[2] = 2;
	for(a=3; a<n+1; a++) {
		perrinArr[a] = perrinArr[a-2] + perrinArr[a-3];
	}

	return perrinArr[n];
}


main() {

	printf("Decimo numero de Perrin \n");

	printf("%d\n", perrin(10));

	printf("%d\n", perrin2(10));

	printf("%d\n", perrin3(10));

}

