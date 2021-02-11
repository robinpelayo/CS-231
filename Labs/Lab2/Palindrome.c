/*	Robin Pelayo
	CS-231
	Lab 2
	Palindrome Checker
*/

/*	The purpose of this program is to input a non-negative number and 
	the program will tell if the number is a palindrome or not. The output
	will be a string. Either 'input_number is a palindrome.' or 'input_number is not a 		
	palindrome.' The program will continue until a negative number is entered.
	
	Psudocode - main:
	Print the title
	Get the input for the first time
	while input is greater than 0 
		Check if input is a palindrome - pass to isPalindrome function
			If so - print "input is palindrome"
		If not - print "input is not plaindrome"
		Get next input

External data table

NAME:		DESCRIPTION:

TRUE		Defined as 1 in this program
FALSE		Defined as 0 in this program
*/ 

#include <stdio.h>

#define FALSE 0
#define TRUE 1

//Function Prototypes
int getInput(); 		//Returns user input
void printTitle();		//Prints the title to the console
int isPalindrome(int number);	//Checks if number is a palindrome - returns 0 if not and 1 if so

void main(){
	printTitle();
	int number = getInput();
	while(number >= 0){
		if (isPalindrome(number))
			printf("\n%d is a palindrome.\n", number);
		else
			printf("\n%d is not a palindrome.\n", number);
		number = getInput();
	}
	
	printf("\nProgram ending...\n");
}

//printTitle definition 
void printTitle(){
/*
	Prints the title to the console 
	This gives the user some breif instructions on how to use the program.
*/
	printf("\t\tPalindrome checker.\n\nEnter a non-negative integer and I will tell you if it's a palindrome or not.\nEnter a negative number when finished.\n");
}

int getInput(){
/*
	Asks the user for input through console message. Then uses
	scanf to get the input and store it into the local variable input. 
	Returns input

DATA TABLE:

NAME:		DESCRIPTION:
input		Holds the user's input number
*/
	int input;
	printf("\nEnter a non-negative integer:\n");
	scanf("%d", &input);
	return input;
}

int isPalindrome(int num){
/*
	Checks if the input argument, num, is a palindrome.
	Returns 1 if num is a palindrome, 0 if not.

	Psudocode - isPalindrome:
	temp variable to hold num
	flip to hold the flipped version of num
	while temp is not 0
		flip is set to flip times 10 plus the remainder of temp devided by 10
		temp is set to the current temp divided by 10
	if flip is equal to num
		return 1
	return 0

DATA TABLE

NAME:		DESCRIPTION:
temp		Temporary variable to hold num
flip		Holds the flipped value of num
*/
	int temp = num;
	int flip = 0;
	while (temp != 0){
		flip = flip*10 + temp%10;
		temp = temp/10;
	}
	return flip == num ? 1: 0;
}









