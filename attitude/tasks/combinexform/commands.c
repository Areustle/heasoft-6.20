#include "commands.h"

#include <stdlib.h>
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <math.h>

#include "att_fatal.h"

#define CHUNK 10
#define COMMAND_LENGTH 128

/******************************************************************************
*
******************************************************************************/
COMMANDS* allocateCommands() {

COMMANDS* list;

list = (COMMANDS*)malloc(sizeof(COMMANDS));

list->commands=NULL;
list->dimen=0;
list->ncommands=0;

return list;

} /* end of allocateCommands */


/******************************************************************************
*
******************************************************************************/
void addCommand(COMMANDS* list, char* command) {

    if(list->ncommands >= list->dimen) {
        /**************************************
        * need to allocate more storage space *
        **************************************/
        list->dimen += CHUNK;
        list->commands = (char**)realloc(list->commands, sizeof(char*)*list->dimen);
    }

    list->commands[list->ncommands++] = command;

} /* end of addCommand function */

/********************************************************************************
*
********************************************************************************/
COMMANDS* readCommandsFromStream(FILE* fp) {

int paren=0;
int c;

char* command;
int i=0;

COMMANDS* list;

list = allocateCommands();


command = (char*)calloc(COMMAND_LENGTH, sizeof(char)*COMMAND_LENGTH);



while((c=fgetc(fp)) != EOF ) {

    if(     c=='(') ++paren;
    else if(c==')') --paren;

    if(i==COMMAND_LENGTH || (isspace(c) && paren==0 && command[0] != '\0')) {
        /*****************
        * end of command *
        *****************/
        command[i]='\0';
        addCommand(list, command);
        command = (char*)calloc(COMMAND_LENGTH, sizeof(char)*COMMAND_LENGTH);
        i=0;
    }

    /******************
    * skip whitespace *
    ******************/
    if(!isspace(c) ) command[i++] = (char)c;

} /* end of loop over characters */
command[i]='\0';
if(command[0] != '\0') addCommand(list, command);

return list;

}/* end of readCommandsFromStream function */


/********************************************************************************
*
********************************************************************************/
COMMANDS* readCommandsFromString(char* string) {

int paren=0;
int c;

char* command;
int i=0;
int j=0;

COMMANDS* list;

list = allocateCommands();


command = (char*)calloc(COMMAND_LENGTH, sizeof(char)*COMMAND_LENGTH);

while((c=string[j++]) != '\0' ) {

    if(     c=='(') ++paren;
    else if(c==')') --paren;

    if(i==COMMAND_LENGTH || (isspace(c) && paren==0 && command[0] != '\0')) {
        /*****************
        * end of command *
        *****************/
        command[i]='\0';
        addCommand(list, command);
        command = (char*)calloc(COMMAND_LENGTH, sizeof(char)*COMMAND_LENGTH);
        i=0;
    }

    /******************
    * skip whitespace *
    ******************/
    if(!isspace(c) ) command[i++] = (char)c;

} /* end of loop over characters */
command[i]='\0';
if(command[0] != '\0') addCommand(list, command);

return list;

}/* end of readCommandsFromStream function */

/******************************************************************************
*
******************************************************************************/
COMMANDS* readCommands(char* source) {

if(source[0] == '@' ) {
    /*******************
    * read from a file *
    *******************/
    FILE* fp;
    fp = fopen(source+1, "r");
    if(fp==NULL) {
        fprintf(stderr, "Could not open %s\n", source+1);
        att_fatal(1);
    }
    
    return readCommandsFromStream(fp);

} else {
    /***********************
    * read from the string *
    ***********************/
    return readCommandsFromString(source);
}



} /* end of readCommands function */

/******************************************************************************
*
******************************************************************************/
COMBOXFORM* executeCommands(COMMANDS* list) {

int i;


char* args;

int length;
char* paren;

COMBOXFORM* combo=NULL;
XFORM2D* trans;

combo=allocateComboXform();
trans = allocateXform2d();

for(i=0; i<list->ncommands; ++i) {

    char* command;
    command = list->commands[i];

    length = strlen(command);
    
    /***********************************************
    * make sure the command ends in a close paren
    * and then clip it off 
    ***********************************************/
    if(command[length-1] != ')' ) {
        fprintf(stderr, "Syntax error: %s\n", command);
        att_fatal(1);
    }
    
    command[length-1] = '\0';
    
    /**************************************************
    * find the open paren and split the command there *
    **************************************************/
    paren = index(command, '(');
    if(paren==NULL) {
        fprintf(stderr, "Syntax error: %s)\n", command);
        att_fatal(1);
    }

    args = paren+1;
    *paren='\0';


    /************************
    * interpret the command *
    ************************/
    if(!strcmp(command, "rot") ) {
        /***********
        * rotation *
        ***********/
        double angle;
        if(sscanf(args, "%lf", &angle) != 1) {
            fprintf(stderr, "Syntax error: %s(%s)\n", command, args);
            att_fatal(1);
        }

        /*********************
        * apply the rotation *
        *********************/
        angle *= M_PI/180.;
        setXform2dToRotation(trans, sin(angle), cos(angle), 0.0, 0.0);
        applyXform2dAfterComboXform(combo, trans);

    } else if(!strcmp(command, "trans") ) {
        /**************
        * translation *
        **************/
        double dx, dy;
        if(sscanf(args, "%lf,%lf", &dx, &dy) != 2) {
            fprintf(stderr, "Syntax error: %s(%s)\n", command, args);
            att_fatal(1);
        }

        /*********************
        * apply the translation *
        *********************/
        setXform2dToTranslation(trans, dx, dy);
        applyXform2dAfterComboXform(combo, trans);
        
    } else if(!strcmp(command, "scale") ) {
        /********
        * scale *
        ********/
        double sx, sy;
        if(sscanf(args, "%lf,%lf", &sx, &sy) != 2) {
            fprintf(stderr, "Syntax error: %s\n", command);
            att_fatal(1);
        }

        /*********************
        * apply the scaling *
        *********************/
        setXform2dToScaling(trans, sx, sy, 0.0, 0.0);
        applyXform2dAfterComboXform(combo, trans);

    } else if(!strcmp(command, "file") ) {
        /*******************************
        * read a transform from a file *
        *******************************/
        COMBOXFORM* file;

        /*********************
        * read the transform *
        *********************/
        file = readComboXform(args);
        if(file==NULL) {
           fprintf(stderr, "Could not read %s\n",args);
            att_fatal(1);
        }

        if(combo->map && file->map) {
            /**********
            * for now *
            **********/
            fprintf(stderr, "Combining non-linear transforms not supported\n");
            att_fatal(1);
        }
        
        /*******************************************
        * depends which one has the non-linear bit *
        *******************************************/
        if(file->map == NULL) {
            /************************
            * the new one is linear *
            ************************/
            applyXform2dAfterComboXform(combo, file->trans);
        } else {
            /********************************************************
            * move the linear transform into the trans temp storage *
            ********************************************************/
            copyXform2d(trans, combo->trans);
            destroyComboXform(combo);
            combo=file;

            /**********
            * combine *
            **********/
            applyXform2dBeforeComboXform(trans, combo);
        }

    } else {
        /**********
        * unknown *
        **********/
        fprintf(stderr, "Unknown command: %s\n", command);
        att_fatal(1);
    }
        


    




} /* end of loop over commands */

destroyXform2d(trans);

return combo;
} /* end of executeCommands function */
