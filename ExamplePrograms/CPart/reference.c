/*  Sean Cotter
		CS-231 Lab Section 2
		Winter 2019
		Recreation of the 'cat' command
*/

/*  Readability note: this document was written with gedit
    with a tab width of 2 spaces */

/*	This program is meant to perfectly emulate the
		'cat' command, which "con'cat'enates" the contents
		of files or input streams like stdin. Command line
		arguments can be the names of files, options (-bEn),
		or a -, which reads from stdin. If no arguments are
		provided, or the only arguments are options, meow
		runs as if - had been included as an argument. The
		program exits when all arguments have been evaluated.

		The options -bEn can be included in any order in the
		arguments and can be repeated. -E will end every line
		with a $. -n will lead every line with a line number.
		-b will lead every non-blank line with a line number.
		If both -b and -n are included as arguments, -b will
		take precedence.

		The program algorithm is:
		
		All arguments are read through and checked for options,
				which will be any of the following:
				-b -bE -bn -bEn -bnE
				-E -Eb -En -Ebn -Enb
				-n -nb -nE -nbE -nEb
		If all arguments are options, input is taken from stdin.
		All arguments are read through again. Options are ignored.
			If the argument is a - and only a -, input is taken
					from stdin.
			If the argument does not start with -, it is identified
					as being a file, and that file is taken as input.
		All output is printed to stdout, which may be specified as
				file from the command line upon invocation.

The external data table
NAME		DESCRIPTION

maxLen	The maximum length of a string allowed as input.

*/



#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define maxLen 1022

void printLine(char [], int [], int *);
void cleanLine(char []);
void concIn(int [], int *);
void concFile(int [], int *, char []);
void optE(char []);


int main(int argc, char * argv []){
/*  main is where the arguments are read and more
    specific functionality is called. The first loop
    checks to see if the arguments are option calls,
    and keeps track of how many arguments were options.
    If the number of options equals the number of
    arguments -1 (argument 0 is the file being run),
    then input is taken only from stdin.

    If there are non-option arguments, then the
    arguments are read again, and stdin or the specified
    file are read as input respectively. 

main data table
NAME		DESCRIPTION

lc			Line count. Tracks line count for -b and -n.
lcp			Line count pointer. Points to lc.
opts		T/F state of the options {-b, -E, -n}.
numOpts	The number of arguments that are options.

*/

	int lc = 1;
	int * lcp = &lc;
	int opts [3] = {0, 0, 0};
	int numOpts = 0;

	for (int i = 1; i < argc; i++){
		if((argv[i][0] == '-') && (argv[i][1] != '\0')){
			numOpts++; // Outside of -bEn assignments in the event of
                 // multiple options in one argument
			// If strchr() returns NULL, the character is not present
			if(strchr(argv[i], 'b') != NULL){
				opts[0] = 1;
			}
			if(strchr(argv[i], 'E') != NULL){
				opts[1] = 1;
			}
			if(strchr(argv[i], 'n') != NULL){
				opts[2] = 1;
			}
		}
	}
	
	// argc includes argv[0], which is just this file
	if(argc == numOpts + 1){
		concIn(opts, lcp);
	}
	else {
	for (int i = 1; i < argc; i++){
		if((argv[i][0] == '-') && (argv[i][1] == '\0')){
			// Argument is not an option. Read from stdin.
			concIn(opts, lcp);
		}
		else if(argv[i][0] != '-'){
			// Argument is not an option or stdin. Read as file.
			concFile(opts, lcp, argv[i]);
		}
	}
	}
}


/*	Concatenate from Stdin.
*/
void concIn(int opts [], int * lcp){
/*  concIn, or 'conc'atenate std'in', takes input from 
    the standard input stream stdin. Each line of input
    is stored in a String (char []) that has a capacity
    2 larger than the max length. This allows for the
    maximum string length while manipulating the end of
    the line with the -E option.

    Using fgets(), input is read from stdin until the 
    EOF (End Of File) signal is detected. In this case,
    the input loop is broken, and stdin is reopened to 
    prevent the overall program from exiting/subsequent
    stdin reads from short-circuiting. line is emptied
    by cleanLine() to prevent overflowing into subsequent
    buffers.

concIn data table
NAME		DESCRIPTION

line		A string containing the current input line.


*/
	char line [maxLen + 2];
	fgets(line, maxLen, stdin);
	while(!feof(stdin)){
		if(feof(stdin)){
			cleanLine(line);
			break;
		}
		else {
			printLine(line, opts, lcp);
			cleanLine(line);
		}
		fgets(line, maxLen, stdin);
	}
	freopen(NULL, "r", stdin);
	return;
}

void concFile(int opts [], int * lcp, char fileName []){
/*  concFile, or 'conc'atenate 'file', reads a file fileName
    line by line until reaching the EOF (End Of File) signal.
    The line String behaves the same way that it does in
    concIn(), but it is populated by the FILE input stream fp
    instead of by stdin.

concFile data table
NAME		DESCRIPTION

fp			A file pointer/input stream. Reads from fileName.
line		A string containing the current input line.

*/
	FILE * fp;
  fp = fopen(fileName, "r");
	char line [maxLen + 2];

	while(!feof(fp)){
		fgets(line, maxLen, fp);
		if(feof(fp)){
			break;
		}
		printLine(line, opts, lcp);
		cleanLine(line);
	}
	fclose(fp);
	return;
}


void printLine(char line [], int opts [], int * lcp){
/*  printLine evaluates any options that were included at
    invocation and directs output of line to stdout. If
    both option -b and -n are set, -b will short-circuit -n.
    -b and -n are evaluated immediately, as their effects
    come before the line being output, which can be printed
    inline immediately after them. -E is evaluated last, as
    it modifies the String and could affect the behaviour
    of -b. For neatness and simplicity, -E's handling is
    packed into its own function.

    The incrementation of the line count is performed
    however the -b and -n options are assigned.

printLine data table
NAME		DESCRIPTION

N/A			N/A
*/
	
	// Option -b
	if(opts[0]){
	// Checks for blank line
		if(line[0] != '\n'){
			printf("	%d ", *lcp);
			*lcp = *lcp + 1;
		}
	}
	// Option -n
	else if(opts[2]){
		printf("	%d ", *lcp);
		*lcp = *lcp + 1;
	}
	// Option -E
	if(opts[1]) {
		optE(line);
	}
	printf("%s", line);
}

void optE(char line []){
/*  optE handles the case of the option -E being set.
    It finds the newline character \n and replaces it 
    with a $. The two characters immediately following
    this location are made into the newline character
    \n and the null character \0 respectively. This
    should never fail, as the line String will always
    have room for 2 extra characters than are read. 
    This is to allow for -E to function, even when the
    buffer is filled.
optE data table
NAME		DESCRIPTION

nlp			Points to where in the line String the newline
   					character is located.
*/
	char * nlp = strchr(line, '\n');
	*nlp = '$';
	*(nlp + 1) = '\n';
	*(nlp + 2) = '\0';
}

void cleanLine(char line[]){
/*  cleanLine steps through the provided line String and
    assigns every char the Null character \0 until the
    end of the String is reached. This is to prevent
    overflow/artifacts from previous lines being output.
    Finally, the standard output is flushed to prevent
    overflow artifacts, which do not affect functionality,
    but make for a consistently more attractive user
    interface.

cleanLine data table
NAME		DESCRIPTION

lp			Line pointer. Steps through the line String.
*/
	char * lp = line;
	while(*lp != '\0'){
		*lp = '\0';
		lp++;
	}
	fflush(stdout);
}

