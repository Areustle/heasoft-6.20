
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#include "textnode.h"
#include "topicnode.h"

char *filename;
FILE *fp;
int line_number;
int eofile;
extern int ihf_error;
extern Topicnode *rootnode;
int using_digits;

/*
 * This function will read the file given by the fname argument and
 * create a topic tree for all topics found therein.
 */

void ihf_rd_help_file(fname)
char *fname;
{
/*    FILE *fopen(); */
    char *next_topic = NULL;
    int next_level = 0;

    void ihf_rdinit(), ihf_mk_topic_tree();

    filename = fname;

    /* Open the file */
    if((fp = fopen(filename,"r")) == NULL) {
        fprintf(stderr,"ihf: Unable to open help file: %s\n",filename);
        fflush(stderr);
        ihf_error = 1;
        return;
    }

    /* Initialize global variables, level and topic buffers, and create
     * the root node and the help node */
    ihf_rdinit(&next_level,&next_topic);

    if(ihf_error) {
        return;
    }

    /* Now read the rest of the help file and finish creating the topic tree */
    ihf_mk_topic_tree(rootnode->subtopic,1,next_level,next_topic);
}

/* 
 * This function, reads the first topic found in the help file and creates
 * the root node for the topic tree from the info found there.  It also
 * creates a help topic node as a subtopic to the root node so that the
 * command 'help help' produces something useful.  Finally, many of the 
 * global variables are initialized.
 */

void ihf_rdinit(next_level,next_topic)
  int *next_level;
  char **next_topic;
{
    char *line, *ihf_rd_line();
    char *roottopic, *ihf_parse_topic();
    Textnode *texthead;

    void ihf_rd_topic_text(), ihf_mk_root_node(), ihf_mk_help_node();

    /* Initialize line counter (global) */
    line_number = 0;

    /* Initialize the end-of-file flag */
    eofile = 0;

    /* Get the first topic line from the help file. Ignore any lines before
     * the first topic line that begin with a space. */
    while( (line = ihf_rd_line()) != (char *) NULL) {

        /* If the line is blank, free it.  Otherwise, we've got the first
         * topic line */
        if(isspace(*line)) {
            free(line);
        }
        else {
            break;
        }

    }

    /* Check for errors */
    if(eofile) {
        ihf_error = 1;
        fprintf(stderr,"ihf: Premature EOF encountered\n");
        fprintf(stderr,"ihf: Help file: %s\n",filename);
        fprintf(stderr,"ihf: Line number: %d\n",line_number);
        fflush(stderr);
        return;
    }

    if(ihf_error) {
        fprintf(stderr,"ihf: Error reading help file\n");
        fprintf(stderr,"ihf: Help file: %s\n",filename);
        fprintf(stderr,"ihf: Line number: %d\n",line_number);
        fflush(stderr);
        return;
    }

    /* Set the using_digits flag (global) */
    if(isdigit(*line)) {
        using_digits = 1;
    }
    else {
        using_digits = 0;
    }

    /* Get the topic name for the root level topic node */
    roottopic = ihf_parse_topic(line);

    /* We don't need line around any longer so free it */
    free(line);

    /* Initialize texthead pointer to NULL */
    texthead = (Textnode *) NULL;

    /* Read all of the root topic text.  Get the next level and topic and 
     * return them to the main routine for use in constructing the rest of
     * of the topic tree. */
    ihf_rd_topic_text(&texthead,next_level,next_topic);

    /* If there were problems reading the root topic text, return */
    if(ihf_error) {
        return;
    }

    /* Now that we have the root topic text and topic, create the root node.
     * rootnode is a global. */
    ihf_mk_root_node(roottopic,texthead);

    /* If there were problems creating the root node, return */
    if(ihf_error) {
        return;
    }

    /* Create a help topic node beneath the rootnode.  This way a user can
     * type 'help help' and get some useful info. ;-) */
    ihf_mk_help_node();

    return;
}

/*
 * Read a line from the help file.  The returned variable points at a
 * malloc'ed copy of the line.
 */

char *ihf_rd_line()
{
    char dmyline[83];
    int  dmylen;
    char *newline;
    static int longwarn = 0;

    /* Return immediately if an error has occurred */
    if(ihf_error) {
        return (char *) NULL;
    }

    /* Read up to 82 bytes of the next line into a temp buffer */
    if(fgets(dmyline,83,fp) == NULL) {
        /* If EOF, Ok; otherwise, read error.  Either way, return NULL */
        if(feof(fp)) {
            eofile = 1; 
        }
        else {
            ihf_error = 1;
        }
        return ((char *)NULL);
    }
    else {
        /* Increment line number if read was successful */
        line_number++;
    }

    /* Malloc the minimal memory necessary to accomodate line */
    dmylen = strlen(dmyline);

    if(dmylen > 81) {
      if(!longwarn) {
        longwarn = 1;
        fprintf(stderr,"ihf: Warning: Line %d is greater than 81 bytes long.\n",line_number);
        fprintf(stderr,"ihf: Warning: Line numbers in error messages may be incorrect.\n");
        fprintf(stderr,"ihf: Warning: Help text may overrun window boundary.\n");
        fprintf(stderr,"ihf: Warning: Help file: %s\n",filename);
        fflush(stderr);
      }
    }

    newline = (char *) malloc((dmylen + 1)*sizeof(char));

    /* Then copy the help file line to new home */
    strcpy(newline,dmyline);
    newline[dmylen] = '\0';

    /* Return pointer to line */
    return newline;
}

/*
 * Parse off the topic name from the help file line.  The returned variable
 * points at a malloc'ed string which contains a copy of the topic name.
 */

char *ihf_parse_topic(line)
char *line;
{
    char *end;
    /*int isalpha();*/
    char *topic;
    int topiclen;

    /* Return immediately if error has occurred */
    if(ihf_error) {
        return (char *) NULL;
    }

    /* move line pointer past topic level indicator */
    if(using_digits) {
        while(isdigit(*line)) {
            line++;
        }
    }
    else {
        while(*line == '*') {
            line++;
        }
    }

    /* Move line pointer past spaces separating level indicator and
     * the topic word */
    while(isspace(*line) && (*line != '\n')) {
            line++;
    }

    /* Move end pointer to end of word.  Spaces cannot be included in a topic
     * name because spaces separate the user's topics on the cl.  
     * e.g. 'help> topic1 topic2'  If spaces were allowed, how would we know 
     * that 'topic1 topic2' is not a single topic name? */
    end = line;
    while(!isspace(*end) && (*end != '\n')) {
        end++;
    }

    /* Get length of topic word */
    topiclen = (int) (end - line);

    if(topiclen == 0) {
        ihf_error = 1;
        fprintf(stderr,"ihf: No topic found on topic line\n");
        fprintf(stderr,"ihf: Help file: %s\n",filename);
        fprintf(stderr,"ihf: Line number: %d\n",line_number);
        fflush(stderr);
        return (char *) NULL;
    }

    /* Malloc memory for topic word and copy to new home */
    topic = (char *) malloc((topiclen+1)*sizeof(char));

    /* Check for errors */
    if(topic == (char *) NULL) {
        fprintf(stderr,"ihf: Out of Memory\n");
        fprintf(stderr,"ihf: Help file: %s\n",filename);
        fprintf(stderr,"ihf: Line number: %d\n",line_number);
        fflush(stderr);
        ihf_error = 1;
        return (char *) NULL;
    }

    /* Copy topic from line to topic buffer */
    strncpy(topic,line,topiclen);
    topic[topiclen] = '\0';

    /* Return pointer to copy of topic */
    return topic;

}

/* 
 * Parse off the level indicator from the help file line.  The returned
 * integer is the topic level for the topic text to follow.  The returned
 * level is adjusted down one level for digit level indicators.  This 
 * adjustment brings the digit format into argeement with the '*' level
 * format. i.e. The first topic in the file is the 0 level topic, etc.
 */
int ihf_parse_level(line) 
char *line;
{
    char *end;
    char *clevel;
    int levellen,ilevel;

    /* Return immediately if error has occurred */
    if(ihf_error) {
        return (-1);
    }

    /* Initialize variable to mark end of level */
    end = line;

    if(using_digits) {
        /* Move end past end of level indicator */
        while(isdigit(*end)) {
            end++;
        }

        /* Malloc a buffer to recieve the level indicator characters and
         * and copy the level to the buffer */
        levellen = (int) (end - line);
        clevel = (char *) malloc((levellen+1)*sizeof(char));

        /* Make sure that memory was available */
        if(clevel == (char *) NULL) {
            fprintf(stderr,"ihf: Out of Memory\n");
            fprintf(stderr,"ihf: Help file: %s\n",filename);
            fprintf(stderr,"ihf: Line number: %d\n",line_number);
            fflush(stderr);
            ihf_error = 1;
            return (-1);
        }

        strncpy(clevel,line,levellen);
        clevel[levellen] = '\0';

        /* Convert the level characters to an integer */
        sscanf(clevel,"%d",&ilevel);

        /* Free the character buffer */
        free(clevel);

        /* Decrement level integer to match shf format level */
        ilevel--;
    }
    else {
        /* Move end past level indicator */
        while(*end == '*') {
            end++;
        }

        /* Level is the number of * characters found. i.e. the length of the
         * level indicator */
        ilevel = (int) (end - line);
    }

    /* Return level as int */
    return ilevel;
}

/*
 * Read help file lines until next topic level indicator is found.  Link all
 * the text into a text node list and assign the first node in the list to
 * texthead.  Since the topic line for the next topic is used to indicate the
 * end of the current topic's text, parse off the level and topic for the next
 * topic and return for later use.
 */

void ihf_rd_topic_text(texthead,next_level,next_topic)
Topicnode **texthead;
int *next_level;
char **next_topic;
{
    char *line,*newline;
    int linelen;

    void ihf_rd_topic_text(), ihf_mk_text_node();
    int ihf_ck_new_topic(char *line);

    /* Return immediately if error occurred */
    if(ihf_error) {
        return;
    }

    /* Read a line from the help file */
    line = ihf_rd_line();

    /* If end-of-file, return */
    if (eofile) return;

    /* If this line contains a topic indicator, we've reached the end of 
     * this section of topic text.  Parse off the topic and level for later
     * use and free the line. */
    if(ihf_ck_new_topic(line)) {
        if(ihf_error) return;
        *next_topic = ihf_parse_topic(line);
        *next_level = ihf_parse_level(line);
        if (ihf_error) return;
        free(line);
    }
    /* If no topic indicator, then add the line to the textnode list */
    else {
        /* If the line begins with a backslash, '\', remove it from the line */
        if(*line == '\\') {
            linelen = strlen(line);
            newline = (char *) malloc(linelen*sizeof(char));
            strcpy(newline,&line[1]);
            newline[linelen-1] = '\0';
            free(line);
            line = newline;
        }

        /* Create a textnode and link in the text line */
        ihf_mk_text_node(texthead,line);

        /* Invoke this routine again to read the next line */
        ihf_rd_topic_text(texthead,next_level,next_topic);
    }

    return;

}

/*
 * Examine the first character of line to see if it contains a
 * topic level indicator.  If so, return true; otherwise, return false.
 */

int ihf_ck_new_topic(line)
char *line;
{

    /* Return immediately if error occurred */
    if(ihf_error) {
        return 0;
    }

    /* Check for level indicator at beginning of line */
    if(using_digits) {
        /* Check for digit in first character of line */
        if(isdigit(*line)) {
            /* Move line pointer past level indicator */
            while(isdigit(*line)) {
                line++;
            }
            /* Move past any spaces separating level and topic */
            while(isspace(*line)) {
                line++;
            }
            /* Look for a single word which follows the level */
            if((isprint(*line)) && (!isspace(*line))) {
                /* Move past the word */
                while((isprint(*line)) && (!isspace(*line))) {
                    line++;
                }
                /* Move past all the spaces following the word */
                while(isspace(*line)) {
                    line++;
                }
                /* This character ought to be nul, otherwise there are
                 * multiple words on this line, and it is not a topic line. */
                if(*line == '\0') {
                    return 1;
                }
            }
        }
    }
    else {
        /* Check for '*' in first character of line */
        if(*line == '*') {
            /* Move past level indicator */
            while(*line == '*') {
                line++;
            }
            /* Move past any spaces separating level indicator and next word */
            while(isspace(*line)) {
                line++;
            }
            /* Look for a single word following the level indicator */
            if((isprint(*line)) && (!isspace(*line))) {
                /* Move past word following level */
                while((isprint(*line)) && (!isspace(*line))) {
                    line++;
                }
                /* Move past any spaces following the word */
                while(isspace(*line)) {
                    line++;
                }
                /* If this character is nul, then there was only one word after
                 * the level, so this must be a topic line. */
                if(*line == '\0') {
                    return 1;
                }
            }
        }
    }

    /* If no level indicator is found, return false */
    return 0;
}

/*
 * Create a text node and link it into the text list.  The location to which
 * texthead points will be written to if nothing is there already.  Line
 * is a pointer to the text which will be linked into the text list.
 */

void ihf_mk_text_node(texthead,line)
Textnode **texthead;
char *line;
{

    Textnode *textnode, *lastnode;

    /* Return immediately if error occurred */
    if(ihf_error) {
        return;
    }

    /* Malloc a new text node */
    textnode = (Textnode *) malloc(sizeof(Textnode));

    /* Make sure memory was allocated */
    if((textnode) == ((Textnode *) NULL)) {
        fprintf(stderr,"ihf: Out of Memory\n");
        fprintf(stderr,"ihf: Help file: %s\n",filename);
        fprintf(stderr,"ihf: Line number: %d\n",line_number);
        fflush(stderr);
        ihf_error = 1;
        return;
    }

    /* If texthead points to nothing, make textnode the head of the list */
    if(*texthead == (Textnode *) NULL) {
        *texthead = textnode;
    }
    /* Otherwise link textnode into the end of the list */
    else {
        /* Make lastnode point to the end of the list */
        for(lastnode= *texthead;lastnode->next;lastnode=lastnode->next) {
        }

        /* Link in textnode */
        lastnode -> next = textnode;
    }

    /* Link the text line into the node */
    textnode -> text = line;

    /* textnode is the end of the list */
    textnode -> next = (Textnode *) NULL;

}

/*
 * Construct and initialize the top level topic node for the ihf topic tree.
 * The top level node is assigned to the global variable rootnode and it
 * is initialized with the arguments topic and texthead.
 */

void ihf_mk_root_node(topic,texthead)
char *topic;
Textnode *texthead;
{

    /* Assign some memory to rootnode (global) */
    rootnode = (Topicnode *) malloc(sizeof(Topicnode));

    /* Check to see if memory was allocated */
    if(rootnode == (Topicnode *) NULL) {
        fprintf(stderr,"ihf: Out of Memory\n");
        fprintf(stderr,"ihf: Help file: %s\n",filename);
        fprintf(stderr,"ihf: Line number: %d\n",line_number);
        fflush(stderr);
        ihf_error = 1;
        return;
    }

    /* Have rootnode's topic point at topic argument */
    rootnode -> topic = topic;

    /* Have rootnode's text point at texthead argument */
    rootnode -> text = texthead;

    /* All other slots for rootnode are filled with NULL's */
    rootnode -> subtopic = (Topicnode *) NULL;
    rootnode -> parent = (Topicnode *) NULL;
    rootnode -> next = (Topicnode *) NULL;

    return;

}

/*
 * Create the help topic node beneath the root node.  All slots for this
 * node are fixed so no arguments are required.  The text for this node
 * is gotten from the header file "helptext.h".
 */

void ihf_mk_help_node()
{
    char *text = 
#include "helptext.h"

    char *topic = "Help";
    Textnode *texthead;

    void ihf_mk_text_node(), ihf_mk_subtopic_node();

    texthead = (Textnode *) NULL;

    ihf_mk_text_node(&texthead,text);

    /* Create the help node as a subtopic to the root node */
    ihf_mk_subtopic_node(rootnode,topic,texthead);

    if(ihf_error) {
        fprintf(stderr,"ihf: Error creating help topic node\n");
        fflush(stderr);
        return;
    }

}

/*
 * Create topic nodes and subtopic nodes as indicated by the help file.  The
 * topic nodes and subtopic nodes are created beneath the topic node pointed 
 * at through the topicnode argument.  The level argument indicates the 
 * topic level for the topicnode argument.  The next_level and next_topic
 * arguments contain info about the next topic to be read from the help file.
 */

void ihf_mk_topic_tree(topicnode,oldlevel,level,topic)
Topicnode *topicnode;
int oldlevel;
int level;
char *topic;
{

    Textnode *texthead;
    int next_level;
    char *next_topic;
    int i;

    void ihf_mk_topic_node(), ihf_mk_subtopic_node();

    if(eofile) {
        return;
    }

    if(ihf_error) {
        return;
    }

    /* The next level needs to be no more than one level below the topicnode
     * level otherwise I can't link it into the topic tree.  It also can't
     * be above the first level lest I define another root level topic node.
     */
    if((level > (oldlevel+1)) || (level < 1)) {
        fprintf(stderr,"ihf: Bad level number: Unable to build topic tree\n");
        fflush(stderr);
        if(using_digits) {
            fprintf(stderr,"ihf: The next topic should have a level greater than 1 and less than %d\n",oldlevel+2);
            fprintf(stderr,"ihf: Found level %d instead\n",level+1);
        }
        else {
            fprintf(stderr,"ihf: The next topic should have a level greater than 0 and less than %d\n",oldlevel+1);
            fprintf(stderr,"ihf: Found level %d instead\n",level);
        }
        fprintf(stderr,"ihf: Help file: %s\n",filename);
        fprintf(stderr,"ihf: Line number %d\n",line_number);
        fflush(stderr);

        ihf_error = 1;
        return;
    }

    /* Initialize the textnode list pointer */
    texthead = (Textnode *) NULL;

    /* Read the text for the next topic found in the help file.  Next_level
     * and next_topic are returned for use in building the upcoming topic
     * node */
    ihf_rd_topic_text(&texthead,&next_level,&next_topic);

    /* By comparing the level and oldlevel arguments, we determine where the
     * topic node should be linked into the topic tree */

    /* Link the topic node somewhere above the topicnode pointer */
    if(level <= oldlevel) {

        /* Make topicnode point at a higher topic node */
        for(i=oldlevel;i>level;i--) {
            topicnode = topicnode -> parent;
        }

        /* Create a topic node next to topicnode */
        ihf_mk_topic_node(topicnode,topic,texthead);

        /* Shift topicnode to point at the newly created topic node */
        topicnode = topicnode -> next;
    }
    /* Link the topic node below the topicnode pointer */
    else {

        /* Create a topic node below topicnode */
        ihf_mk_subtopic_node(topicnode,topic,texthead);

        /* Make topicnode point at the new subtopic node */
        topicnode = topicnode -> subtopic;
    }

    /* Call this routine again to continue reading and creating topic nodes */
    ihf_mk_topic_tree(topicnode,level,next_level,next_topic);

}

/*
 * Create a new topic node next to the topic node pointed at by the topicnode
 * argument.  Fill the topic node slots with the pointers to the topic string
 * and the text node list using the topic and texthead arguments, respectively.
 */

void ihf_mk_topic_node(topicnode,topic,texthead)
Topicnode *topicnode;
char *topic;
Textnode *texthead;
{

    Topicnode *newtopicnode;

    /* Allocate enough memory for a new topic node */
    newtopicnode = (Topicnode *) malloc(sizeof(Topicnode));

    /* If no memory available, print error message and return */
    if(newtopicnode == (Topicnode *) NULL) {
        fprintf(stderr,"ihf: Out of Memory\n");
        fprintf(stderr,"ihf: Help file: %s\n",filename);
        fprintf(stderr,"ihf: Line number: %d\n",line_number);
        fflush(stderr);
        ihf_error = 1;
        return;
    }

    /* Link the newtopicnode next to the topic node pointed at by topicnode */
    topicnode -> next = newtopicnode;

    /* Assign values to the topic node slots */
    newtopicnode -> topic = topic;
    newtopicnode -> text = texthead;

    /* The parent of the new topic node is also the parent of topicnode */
    newtopicnode -> parent = topicnode -> parent;

    /* Cap the pointers to other topic nodes */
    newtopicnode -> subtopic = (Topicnode *) NULL;
    newtopicnode -> next = (Topicnode *) NULL;

}

/*
 * Create a new topic node under the topic node pointed at by the topicnode
 * argument.  Fill the topic node slots with the pointers to the topic string
 * and the text node list using the topic and texthead arguments, respectively.
 */

void ihf_mk_subtopic_node(topicnode,topic,texthead)
Topicnode *topicnode;
char *topic;
Textnode *texthead;
{
    
    Topicnode *newtopicnode;

    /* Allocate enough memory for a new topic node */
    newtopicnode = (Topicnode *) malloc(sizeof(Topicnode));

    /* If no memory available, print error message and return */
    if(newtopicnode == (Topicnode *) NULL) {
        fprintf(stderr,"ihf: Out of Memory\n");
        fprintf(stderr,"ihf: Help file: %s\n",filename);
        fprintf(stderr,"ihf: Line number: %d\n",line_number);
        fflush(stderr);
        ihf_error = 1;
        return;
    }

    /* Link the newtopicnode under the topic node pointed at by topicnode */
    topicnode -> subtopic = newtopicnode;

    /* Assign values to the topic node slots */
    newtopicnode -> topic = topic;
    newtopicnode -> text = texthead;

    /* The parent of the new topic node is the topicnode */
    newtopicnode -> parent = topicnode;

    /* Cap the pointers to other topic nodes */
    newtopicnode -> subtopic = (Topicnode *) NULL;
    newtopicnode -> next = (Topicnode *) NULL;

}
