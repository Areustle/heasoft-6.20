
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#ifdef unix

#include "readline/readline.h"
#include "readline/history.h"

#endif

#include "textnode.h"
#include "topicnode.h"
#include "topiclistnode.h"

Topiclistnode *topiclisthead, *topiclistptr, *topicpathhead;
char *line, *linehead;

extern Topicnode *rootnode;
extern int ihf_error;

Topicnode *current_topic_node;

/*
 * This is the top level function for the ihf user interface.  The topics 
 * argument is a pointer to the topics the user gave when invoking the 
 * help command.  If it's NULL, then the user gave no initial topics.
 */

void ihf_be_helpful(topics)
char *topics;
{
    void ihf_give_help_init(), ihf_give_help_cleanup(), ihf_give_help();

    /* Prepare the variables used when talking to the user */
    ihf_give_help_init(topics);

    /* If the user gave no initial topics, then display the root level
     * help node to the user.  Otherwise, don't bother, they know where
     * they want to go. */
    if(topics == (char *) NULL) {
        ihf_give_help(rootnode,1,1);
    }
    else {
        ihf_give_help(rootnode,0,0);
    }

    /* Free memory and tidy up */
    ihf_give_help_cleanup();
}

/*
 * This function initializes the variables used by the user interface.  If
 * the user typed some topics on the command line, they are formed into the
 * topiclist.
 */
void ihf_give_help_init(topics) 
char *topics;
{

    int topiclen;
    char *ihf_stripsp();

    void ihf_mk_topic_list(), ihf_push_topic_path();

    /* Initialize the two topic list pointers to NULL. */
    topiclisthead = (Topiclistnode *) NULL;
    topiclistptr = (Topiclistnode *) NULL;

    /* Initialize the topicpath and assign the root topic as the first topic
     * in the topicpath. */
    topicpathhead = (Topiclistnode *) NULL;
    ihf_push_topic_path(rootnode->topic);

    /* If the user didn't give us any topics when first calling the ihf
     * function, we don't need to initialize anything else */
    if(topics == (char *) NULL) {
        return;
    }

    /* Get length of topic string */
    topiclen = strlen(topics);

    /* Copy the line to a memory location which I can free later */
    linehead = (char *) malloc((topiclen+1)*sizeof(char));

    strcpy(linehead,topics);
    linehead[topiclen] = '\0';

    /* Strip spaces from the front and back of the user's line */
    line = ihf_stripsp(linehead);

    /* Create a topic list from the user's line. (Assigns head 
     * to topiclisthead) */
    ihf_mk_topic_list();

    /* Make topiclistptr also point to the list head */
    topiclistptr = topiclisthead;
}

/*
 * This function frees memory which was allocated, but not needed anymore.
 */

void ihf_give_help_cleanup()
{

    void ihf_free_topic_list(), ihf_pop_topic_path();

    /* Free the user interface buffer */
    if(linehead != (char *) NULL) {
        free(linehead);
        linehead = NULL;
    }

    /* Free the topiclist */
    ihf_free_topic_list();

    /* Free the topicpath */
    while(topicpathhead != (Topiclistnode *) NULL) {
        ihf_pop_topic_path();
    }
}

/*
 * This function is the center piece of the user interface.  It provides help
 * for the topic given in the topicnode argument.  The withtext and withsubs
 * arguments flag whether the topic text and it's sutopics need to be 
 * displayed.  After any display items are shown, the next topic is gotten
 * from the topic list and acted upon.
 */

void ihf_give_help(topicnode,withtext,withsubs)
Topicnode *topicnode;
int withtext;
int withsubs;
{

    char *topic;
    int mode;
    int moretopics;
    Topicnode *dmynode, *ihf_find_subtopic();
    int gotinput,textflag,subsflag;

    void ihf_give_help(), ihf_display_text(), ihf_display_subs();
    void ihf_gt_next_topic(), ihf_push_topic_path(), ihf_pop_topic_path();
    void ihf_disp_topic_path();
    int ihf_gt_topic_mode();

    /* If this function is called with a NULL topicnode, then begin exiting
     * from the ihf */
    if(topicnode == (Topicnode *) NULL) {
        return;
    }

    /* Display the topic path only if the text or the subtopics 
     * need to be shown */
    if(withtext || withsubs) {
        fprintf(stdout,"\n");
        ihf_disp_topic_path();
        fflush(stdout);
    }

    /* Display the topic text, if needed and only if there's some to show */
    if(withtext) {
        if((topicnode -> text) != (Textnode *) NULL) {
            fprintf(stdout,"\n");
            ihf_display_text(topicnode);
            fflush(stdout);
        }
    }

    /* Display the available subtopics, if needed and only if there's some
     * to show. */
    if(withsubs) {

        if((topicnode -> subtopic) != (Topicnode *) NULL) {
            fprintf(stdout,"\n");
            fprintf(stdout,"Available Subtopics: \n");
            ihf_display_subs(topicnode->subtopic);
            fflush(stdout);
        }
    }

    /* The readline function may get called in ihf_gt_next_topic, so
     * we need to set current_topic_node (global variable) */
    current_topic_node = topicnode;

    /* Get the next topic in the topic list.  This routine talks to the 
     * user if needed. */
    ihf_gt_next_topic(&topic,&moretopics);

    /* Figure out what came back from the user */
    mode = ihf_gt_topic_mode();

    /* There are four possible responses to a prompt.  Each requires a 
     * different action:
     *
     * 1) <return> (including a space filled line) - move to parent topic
     * 2) <EOF>                                    - exit ihf package
     * 3) a subtopic                               - search for subtopic match
     * 4) '?'                                      - redisplay current topic
     */

    switch(mode) {

        case 1: /* <return> - move to parent topic */
                
                /* Remove last topic from topic path */
                ihf_pop_topic_path();

                /* Display the parent topic's subtopics, but not the text, 
                 * then get the next topic from user */
                ihf_give_help(topicnode -> parent,0,1);

                /* Since ihf_give_help is recursive, it's invocation must be
                 * followed by a return */
                return;

        case 2: /* <EOF> - exit from ihf package */

                return;

        case 3: /* a subtopic - search for matching subtopic */

                /* Look for a matching subtopic and assign to dmynode */
                dmynode = ihf_find_subtopic(topic,topicnode,&gotinput);

                /* If no match was found, then dmynode is NULL and user will
                 * need to be reprompted */
                if(dmynode == (Topicnode *) NULL) {

                  /* If we didn't talk to the user when searching for a match,
                   * give them an error message.  Otherwise they already know
                   * what's going on. */
                  if(!gotinput) {
                    fprintf(stderr,"No subtopic found matching '%s'\n",topic);
                    fflush(stderr);
                  }

                  /* Have next call to ihf_give_help print available subtopics
                   * but not the help text */
                  textflag = 0;
                  subsflag = 1;

                  /* Use the current node for the next ihf_give_help call */
                  dmynode = topicnode;

                  /* Make topiclist pointer point at NULL.  This will cause
                   * any topics that are queued up in the topiclist to be 
                   * deleted with the next ihf_gt_next_topic call */
                  if(topiclistptr != (Topiclistnode *) NULL) {
                      topiclistptr = (Topiclistnode *) NULL;
                  }
                }

                /* Otherwise a match was found */
                else{

                    /* Put the next topic on the topic path */
                    ihf_push_topic_path(dmynode->topic);

                    /* If there are more topics in the topiclist queue, 
                     * then don't display the text and subtopics */
                    if(moretopics) {
                        textflag = 0;
                        subsflag = 0;
                    }
                    /* Otherwise, the user is at a node where they need to
                     * see some help info */
                    else {
                        textflag = 1;
                        subsflag = 1;
                    }
                }

                /* Reprompt the user using all the flags and variables as
                 * set above */
                ihf_give_help(dmynode,textflag,subsflag);
                return;

        case 4: /* '?' - redisplay text and subtopics for current topicnode */

                /* If any topics were given on the command line after the '?'
                 * see that they are deleted the next time ihf_gt_next_topic
                 * is called */
                topiclistptr = NULL;

                /* Redisplay the current topicnode */
                ihf_give_help(topicnode,1,1);
                return;
    }

}

/* 
 * Searches for a subtopic under the topicnode argument which matches the 
 * topic argument.  The comparison is case insensitive so copies of each 
 * subtopic topic and the topic argument are created.  Each copy is converted
 * to lower case for the comparison.  As each subtoic is examined, a list of
 * all matching subtopic nodes is kept.  If there is more than one match, 
 * the list is displayed and the user prompted.  When the user is prompted
 * the gotinput argument is returned as true.
 */

/* Modification history:
 *     James Peachey, HEASARC/GSFC/NASA, Raytheon STX, 19 February, 1998
 *     Fixed so that an exact match to a user's help choice is recognized
 *     even if more than one possible match is found. jp 19-feb-1998
 */

Topicnode *ihf_find_subtopic(topic,topicnode,gotinput)
char *topic;
Topicnode *topicnode;
int *gotinput;
{

    int nmatches = 0, topiclen, dmyflag;
    Topicnode *subnode,*newnode,*listhead,*lastnode,*dmynode,*nextnode;
    char *input, *inputptr, *ihf_rl(), *ihf_stripsp();
    char *topiccopy,*subtopiccopy,*c;
    int subtopiclen;
    int exactmatch = 0; /* jp 19-feb-1998 */

    void ihf_display_subs();

    /* Initialize got input to false */
    *gotinput = 0;

    /* Initialize the list which records all the topicnodes matching topic.
     * Although a more simple structure can be created for the nodes of this
     * list, the Topicnode structure will do just as well */
    listhead = (Topicnode *) NULL;

    /* Make a copy of topic argument and lower case it */
    topiclen = strlen(topic);
    topiccopy = (char *) malloc((topiclen+1)*sizeof(char));
    strcpy(topiccopy,topic);
    topiccopy[topiclen] = '\0';
    for(c=topiccopy; *c != '\0'; c++) {
        *c = tolower(*c);
    }

    /* Search the subtopics for a matching topic.  When found, save a 
     * pointer to it in our temporary Topicnode list */
    for(subnode=(topicnode->subtopic);subnode!=(Topicnode *)NULL;
    subnode=(subnode->next)) {

        /* Create a lowercase copy of the subtopic topic */
        subtopiclen = strlen(subnode->topic);
        subtopiccopy = (char *) malloc((subtopiclen+1)*sizeof(char));
        strcpy(subtopiccopy,subnode->topic);
        subtopiccopy[subtopiclen] = '\0';
        for(c=subtopiccopy; *c != '\0'; c++) {
            *c = tolower(*c);
        }

        /* if a match is found, save a pointer to the topic node */
        if(strncmp(topiccopy,subtopiccopy,topiclen) == 0) {

            /* Check for an exact match jp 19-feb-1998 */
            if(strlen(topiccopy) == strlen(subtopiccopy)) {
                exactmatch = 1;
            }

            /* Increment the number of matches variable */
            nmatches++;

            /* Create a new node for our temporary Topicnode list.  Save the
             * subtopic topic in the topic slot, and the subtoic node in the
             * subtopic slot. */
            newnode = (Topicnode *) malloc(sizeof(Topicnode));
            newnode -> topic = subnode -> topic;
            newnode -> next = (Topicnode *) NULL;
            newnode -> subtopic = subnode;

            /* Link the new node into our temporary list */
            if(listhead == (Topicnode *) NULL) {
                /* If the listhead is empty, make it point at the new node */
                listhead = newnode;
            }
            else {
                /* Otherwise, link it into the last node on the list */
                for(lastnode=listhead;(lastnode->next)!=(Topicnode *) NULL;
                lastnode=lastnode -> next) {
                }
                lastnode -> next = newnode;
            }
        }
        /* The copy of the subtopic topic is not needed anymore */
        free(subtopiccopy);
        if(exactmatch) break; /* jp 19-feb-1998 */
    }
    /* The copy of the topic argument is not needed anymore */
    free(topiccopy);


    /* jp 19-feb-1998: If there was an exact match, or */
    /* If there was only one match found above, return the topicnode from the
     * matching subtopic */
    if(nmatches == 1 || exactmatch) {

        /* Record the matching topic node in a temporary variable */
        dmynode = listhead -> subtopic;

        /* Free the Topicnode list of matches */
        free(listhead);

        /* Return the pointer to the matching topic node */
        return dmynode;
    }

    /* If there were multiple matches, display the matches and 
     * prompt the user */
    if(nmatches > 1) {

        /* Set the gotinput argument to true */
        *gotinput = 1;

        /* Tell the user that there's a problem, and display the matching
         * subtopics */
        fprintf(stdout,"Multiple subtopics match '%s'.  Please make a unique selection:\n",topic);
        ihf_display_subs(listhead);
        fflush(stdout);

        /* Get input from the user */
        input = ihf_rl("subtopic> ");

        /* Strip spaces from the user's input */
        inputptr = ihf_stripsp(input);

        /* Free the Topicnode list of matches */
        for(dmynode=listhead;dmynode!=(Topicnode *) NULL;dmynode=nextnode) {
            nextnode = dmynode -> next;
            free(dmynode);
        }

        /* If the user gave us EOF or an empty string, return NULL */
        if((inputptr == (char *) NULL) || (inputptr[0] == '\0')) {
            free(input);
            return ((Topicnode *) NULL);
        }
        /* Otherwise locate the subtopic match for the given topic */
        else {
            dmynode = ihf_find_subtopic(inputptr,topicnode,&dmyflag);
            free(input);
            return (dmynode);
        }
    }

    /* If nothing matched, return NULL */
    if(nmatches == 0) {
        return ((Topicnode *) NULL);
    }

    /* If we have reached the end of the function without returning then return NULL */
    return  ((Topicnode *) NULL);

}

/*
 * Displays the help text found in the text slot of the topicnode argument.
 * Uses the ihf_display function to scroll the output
 */

void ihf_display_text(topicnode)
Topicnode *topicnode;
{
    int init;
    Textnode *textnode;

    int ihf_display(char *text, int init);

    init = 1;

    /* Display each node in the Textnode list. */
    for(textnode=(topicnode->text);textnode!=(Textnode *) NULL;
    textnode=(textnode->next)){
        /* Continue displaying the text until the user types EOF.  The init
         * flag resets the display line counter */
        if(!ihf_display(textnode->text,init)) {
            return;
        }
        init = 0;
    }
}

/*
 * Displays the subtopics pointed to by the subtopicnode argument.  All the
 * subtopics which occur on the same level and after the subtopicnode argument
 * are displayed.  The topic text is columnated with the longest topic 
 * giving the column width.
 */

void ihf_display_subs(subtopicnode)
Topicnode *subtopicnode;
{
    int ihf_display(char *text, int init);

    Topicnode *subnode;
    int maxtopiclen,topiclen,ntopics,ntpl,init,displinelen;
    int tcount,i;
    char displine[82];

    init = 1;

    maxtopiclen = 0;
    ntopics = 0;

    /* Find the maximum length and total number of topics to be displayed */
    for(subnode=subtopicnode;subnode!=(Topicnode *)NULL;subnode=subnode->next) {

        topiclen = strlen(subnode->topic);
        if(maxtopiclen < topiclen){
            maxtopiclen = topiclen;
        }
        ntopics++;
    }

    /* Add 2 to the maxtopiclen for the '  ' appended to each topic when 
     * displayed */
    maxtopiclen = maxtopiclen + 2;

    /* Calculate the number of topics per line */
    ntpl = (int) ((79+2)/maxtopiclen);

    /* Reset the subnode variable to point at the first topicnode in the list */
    subnode = subtopicnode;

    /* As long as we're not at the end of the list, keep displaying subtopics */
    while(subnode != (Topicnode *) NULL) {

        /* Initialize display line */
        displine[0] = '\0';

        /* Put ntpl number of topics on the display line with a '  ' after 
         * each. Pad the end of each topic word to make it as long as the
         * longest topic. */
        for(tcount=0;(tcount<ntpl)&&(subnode!=(Topicnode *) NULL);tcount++) {

            /* Write the topic to the end of the display line */
            displinelen = strlen(displine);
            strcat(displine,subnode -> topic);
            topiclen = strlen(subnode -> topic);
            displine[displinelen + topiclen] = '\0';
            strcat(displine,"  \0");

            /* Pad the end with spaces */
            for(i=topiclen+2;i<maxtopiclen;i++) {
                strcat(displine," \0");
            }

            /* Move the subnode variable on to the next topicnode */
            subnode = subnode -> next;
        }

        /* The last character of the display line is just a space; replace it
         * with a newline character */
        displine[strlen(displine)-1] = '\n';

        /* Display the line we've built unless the user returns EOF */
        if(!ihf_display(displine,init)) {
            return;
        }
        init = 0;

    }

}

/*
 * Generic displayer for the ihf_display_subs and ihf_display_text functions.
 * Display 23 lines of text at a time, then prompt the user to continue.  No
 * newline character is appended to the text argument.  The init argument 
 * causes the 23 line counter to reinitialize.
 */

int ihf_display(text,init)
char *text;
int init;
{
    static int counter;
    char *buffer;

    /* Reinitialize the counter variable if asked */
    if(init) {
       counter = 0;
    }

    /* If we've displayed 23 lines of text already, reset the counter variable
     * and prompt the user to continue */
    if(counter == 23) {

        /* Reset counter */
        counter = 0;

        /* Prompt the user to continue */
        fprintf(stdout,"\n");
        fprintf(stdout,"Type <return> to continue ...\n");
        fflush(stdout);

        /* Allocate a buffer for the user's response */
        buffer = (char *) malloc(20*sizeof(char));
        if(fgets(buffer,20,stdin) == (char *) NULL ) {
            free(buffer);
            return 0;
        }

        free(buffer);

    }

    /* Increment counter variable */
    counter++;

    /* Display the text argument */
    fprintf(stdout,"%s",text);
    fflush(stdout);

    return 1;
        
}

/* Return the next topic in the topiclist list.  If the topic list is empty
 * ask the user for more topics.  The moretopics argument is set to true when
 * there are more topics in the topic list.  This occurs when the user types
 * multiple topics on the cl.
 */

void ihf_gt_next_topic(topic,moretopics)
char **topic;
int *moretopics;
{

    void ihf_gt_topic_list();

    /* If the topiclist pointer points at nothing, get more topics 
     * from the user */
    if(topiclistptr == (Topiclistnode *) NULL) {
        /* Get topics from user */
        ihf_gt_topic_list();

        /* Reset topiclist pointer to point at first topic in list */
        topiclistptr = topiclisthead;
    }

    /* If user typed <EOF>, topiclistptr will be NULL, so return with 
     * NULL topic */
    if(topiclistptr == (Topiclistnode *) NULL) {

        /* Set topic to NULL */
        *topic = (char *) NULL;

        /* There's no more topics in the list */
        *moretopics = 0;

        return;
    }

    /* If we got here then, user typed in some text.  Set return topic arg */
    *topic = topiclistptr -> topic;

    /* If the next item in the list is NULL, then there's no more items 
     * in the list */
    if((topiclistptr -> next) == (Topiclistnode *) NULL) {
        *moretopics = 0;
    }
    else {
        *moretopics = 1;
    }

    /* Move the topiclist pointer on to the next topic in the list */
    topiclistptr = topiclistptr -> next;
}

/*
 * Deciphers the user's response by examining line.  Four possible responses:
 * 1) <EOF> - line = NULL
 * 2) <return> (including space filled line) - line = '\0'
 * 3) words - line != '\0'
 * 4) '?' - line = '?'
 */

int ihf_gt_topic_mode()
{
    if(line == (char *) NULL) {
        return 2;
    }
    else if(line[0] == '\0') {
        return 1;
    }
    else if(line[0] == '?') {
        return 4;
    }
    else {
        return 3;
    }
}

/*
 * Prompts user for input and forms a topiclist from their response.  Resets
 * the topiclist, and line global pointers.
 */

void ihf_gt_topic_list()
{

    char *ihf_rl(), *ihf_stripsp();

    char promptstring[100];

    void ihf_mk_topic_list(), ihf_free_topic_list();

    /* Free all the topic list entries */
    ihf_free_topic_list();

    /* Since the topiclist is now empty, reset the topiclist head 
     * pointer to NULL */
    topiclisthead = (Topiclistnode *) NULL;

    /* Free the memory used when last talking to the user */
    if(linehead != (char *) NULL) {
        free(linehead);
        linehead = NULL;
    }

    /* Prompt the user for input */
    strcpy(promptstring,"\nhelp> ");
    linehead = ihf_rl(promptstring);

    /* If user entered <EOF>, begin exiting the ihf package */
    if(linehead == (char *) NULL) {
        /* Set line to NULL */
        line = NULL;

        /* Print a newline so the next prompt given to user looks alright */
        fprintf(stdout,"\n");
        fflush(stdout);

        return;
    }

    /* Strip spaces from front and back of user's response */
    line = ihf_stripsp(linehead);

    /* If user typed some topics on the cl, save the cl in the history list,
     * and create a topic list from it */
    if(line[0] != '\0') {

#ifdef unix
        /* Save the command line in the history list */
        add_history(line);

#endif
        /* Create a topiclist */
        ihf_mk_topic_list();
    }
}

/*
 * Creates a linked list of topics found in the line variable.  The list head
 * location is written to the topiclisthead variable.  
 */

void ihf_mk_topic_list()
{
    char *word;
    int wordlen;
    int bol;  /*beginning of line flag*/
    Topiclistnode *topiclistnode;
    char *topic;

    /* Set the topiclisthead to NULL */
    if(topiclisthead != (Topiclistnode *) NULL) {
        fprintf(stderr,"ihf: Improper initialization\n");
        fprintf(stderr,"ihf: Topiclisthead not NULL before using ihf_mk_topic_list\n");
        fflush(stderr);
        ihf_error = 1;
        return;
    }

    wordlen = 0;
    bol = 0;

    /* Set word to point at last letter in last word of line */
    word = line + strlen(line) - 1;

    /* Start at the end of the line and parse words.  When beginning of line
     * is found, stop. */
    while(!bol) {


        /* If we got to the beginning of line, set bol flag */
        if(word == line) {
            bol = 1;
            wordlen++;
        }
        /* Otherwise, if we haven't reached the beginning of a word, move
         * back one character and continue parsing the line. */
        else if(*word != ' ') {
                wordlen++;
                word--;
                continue;
        }

        /* If we got here, then we need to create another node 
         * in the topic list. */
        topiclistnode = (Topiclistnode *) malloc(sizeof(Topiclistnode));

        if(topiclistnode == (Topiclistnode *) NULL) {
            fprintf(stderr,"ihf: Out of Memory\n");
            fprintf(stderr,"ihf: Failed while attempting to create topic list node\n");
            fflush(stderr);
            ihf_error = 1;
            return;
        }

        /* If the topiclisthead points to nothing, then this node becomes the
         * list head. */
        if(topiclisthead == (Topiclistnode *) NULL) {
            topiclistnode -> next = (Topiclistnode *) NULL;
            topiclisthead = topiclistnode;
        }
        /* Otherwise, link this node into the top of the list */
        else {
            topiclistnode -> next = topiclisthead;
            topiclisthead = topiclistnode;
        }

        /* Allocate space to hold the word */
        topic = (char *) malloc((wordlen+1)*sizeof(char));

        /* Copy the word */
        if(bol) {
            strncpy(topic,word,wordlen);
        }
        else {
            strncpy(topic,&word[1],wordlen);
        }
        topic[wordlen] = '\0';

        /* Link the word into the node */
        topiclistnode -> topic = topic;

        /* Reset the word length variable for the next word. */
        wordlen = 0;

        /* Move the word pointer to the last letter of the next word */
        while(*word == ' ') {
            word--;
        }

    }
}

/*
 * Frees the topiclist starting at the topiclist head.
 */

void ihf_free_topic_list()
{

    Topiclistnode *topiclistnode, *nextnode;

    for(topiclistnode=topiclisthead;topiclistnode != (Topiclistnode *)NULL;
    topiclistnode = nextnode) {
        nextnode = topiclistnode -> next;
        free(topiclistnode -> topic);
        free(topiclistnode);
    }
}

/*
 * Create a copy of the topic argument and link it into the end of the 
 * topic path list.
 */

void ihf_push_topic_path(topic)
char *topic;
{
    int topiclen;
    char *topiccopy;
    Topiclistnode *pathnode,*lastnode;

    /* Create a copy of the topic argument */
    topiclen = strlen(topic);
    topiccopy = (char *) malloc((topiclen+1)*sizeof(char));
    strcpy(topiccopy,topic);
    topiccopy[topiclen] = '\0';

    /* Create a new topic path node and link in the topic copy */
    pathnode = (Topiclistnode *) malloc(sizeof(Topiclistnode));
    pathnode -> next = NULL;
    pathnode -> topic = topiccopy;

    /* Link the new topic path node into the topic path list */
    if(topicpathhead == (Topiclistnode *) NULL) {
        /* If the topic path list is empty, then the new path node is the 
         * first node in the list */
        topicpathhead = pathnode;
    }
    else {
        /* Otherwise, move to the last node of the list and link the new node
         * after the last node */
        for(lastnode=topicpathhead;(lastnode->next)!=(Topiclistnode *)NULL;
        lastnode=lastnode->next) {
        }
        lastnode -> next = pathnode;
    }
}

/* 
 * Delete the last node in the topic path list.
 */
void ihf_pop_topic_path()
{
    Topiclistnode *next2last,*lastnode;

    next2last = (Topiclistnode *) NULL;

    /* find the last node in the topic path list and the next to last node */
    for(lastnode=topicpathhead;(lastnode->next)!=(Topiclistnode *)NULL;
    lastnode=lastnode->next) {
        next2last = lastnode;
    }

    /* Release the memory used by the topic */
    free(lastnode -> topic);

    /* Set the topicpath head appropriately if the last node in the list 
     * is the list head */
    if(lastnode == topicpathhead) {
        topicpathhead = (Topiclistnode *) NULL;
    }

    /* Release the memory used by the topic path node */
    free(lastnode);

    /* The next to last node in the list is now the last node.  Cap the end
     * of the list */
    if(next2last != (Topiclistnode *) NULL) {
        next2last -> next = (Topiclistnode *) NULL;
    }
    
}

/*
 * Displays the topic path list.  Each topic gets it's own line and is
 * indented 5 spaces for every printed ahead of it.
 */

void ihf_disp_topic_path()
{
    int init,tabcount,i,displen;
    char displine[82];
    Topiclistnode *pathnode;
    int topiclen, maxtopiclen = 0;

    /* Initialize display variables */
    init = 1;
    tabcount = 0;
    displine[0] = '\0';
    displen = 0;

    /* Find the maximum topic length for all the topics to be displayed */
    for(pathnode=topicpathhead;pathnode!=(Topiclistnode *)NULL;
    pathnode=pathnode->next) {
        topiclen = strlen(pathnode->topic);
        if(maxtopiclen < topiclen) {
            maxtopiclen = topiclen;
        }
    }

    /* Display each topic */
    for(pathnode=topicpathhead;pathnode!=(Topiclistnode *)NULL;
    pathnode=pathnode->next) {

        /* Each line gets an additional indentation, so count the 
         * number of lines printed */
        tabcount++;

        /* Prepend each line with the appropriate number of 5 space 
         * indentations */
        for(i=0;i<tabcount;i++) {
            /* Stop prepending indentations if the displine buffer 
             * will overflow */
            if(strlen(displine) > (80-maxtopiclen-5)) {
                break;
            }
            /* Prepend the indentations */
            strcat(displine,"     \0");
        }

        /* Tack on the topic to the display line */
        displen = strlen(displine);
        strcat(displine,pathnode->topic);
        displine[displen+strlen(pathnode->topic)] = '\n';
        displine[displen+strlen(pathnode->topic)+1] = '\0';

        /* Display the topic */
        ihf_display(displine,init);
        init = 0;

        /* Reset the display line buffer */
        displine[0] = '\0';
        displen = 0;
    }
}

/*
 * Strip spaces from the front and back of the string argument.  Return a
 * pointer to the first non-space byte in the string argument, and place a
 * null after the last non-space byte in the string argument.
 */

char *ihf_stripsp(string)
char *string;
{
    char *front, *end;

    /* Return garbage if given garbage */
    if(string == (char *) NULL) {
        return (char *) NULL;
    }

    /* Move front to the first word in line */
    for(front=string;isspace(*front);front++) {
    }

    /* If no words were in line, front points at null. Set first char to null
     * and return pointer to it */
    if(*front == '\0') {
        string[0] = '\0';
        front = string;
        return string;
    }

    /* Move end to the end of the last word in line */
    for(end=string+strlen(string)-1;isspace(*end);end--) {
    }

    /* Terminate the line after the end of the last word */
    *(++end) = '\0';

    return front;
}

/*
 * This is a wrapper to the readline call.  On vms systems, this function
 * contains a call to fgets.  On unix systems, readline is used.
 */

char *ihf_rl(prompt)
char *prompt;
{
#ifdef unix
    /* If unix use readline */
    return (readline(prompt));
#else
    char buffer[256], *str;
    int buflen;

    /*Otherwise, print the prompt for the user to see */
    fprintf(stdout,"%s",prompt);
    fflush(stdout);

    /* Read the user's input */
    if(fgets(buffer,256,stdin) != (char *) NULL) {

        /* Put a newline after the user's response so that the next prompt
         * looks ok */
        fprintf(stdout,"\n");
        fflush(stdout);

        /* Allocate space for the user's response and copy it there */
        buflen = strlen(buffer);
        str = (char *) malloc((buflen+1)*sizeof(char));
        strcpy(str,buffer);
        str[buflen] = '\0';

        return (str);
    }
    else {
        /* Put a newline after the user's response so that the next prompt
         * looks ok */
        fprintf(stdout,"\n");
        fflush(stdout);

        return (char *) NULL;
    }

#endif
}

/*
 * This function is called by readline when the user tries to complete
 * a subtopic on the command line.  It returns one item from the list of 
 * possible completions for the text argument.  Each subsequent call
 * returns the next item in the list.  The global variable current_topic_node
 * should be set appropriately when the readline function is called.
 * Text is the user supplied substring which is being completed.
 * State is zero when this routine is first called, and positive with
 * subsequent calls. Only compiled on unix systems.
 */

#ifdef unix

char *ihf_topic_generator(text,state)
char *text;
int state;
{
    static int textlen;
    static char *textcopy;
    static Topicnode *subtopic;
    char *topic;
    int topiclen;
    char *dmystr,*topiccopy,*c;

    void reset_subtopic();

    /* If first call to generator, initialize topic node pointer */
    if (!state) {
        subtopic = current_topic_node -> subtopic;

        textlen = strlen(text);

        /* Move topic node pointer to appropriate subtopic as indicated by
         * topics listed on the current command line */

        reset_subtopic(&subtopic);

        /* Create a lower-case copy of readline's completion text so 
         * case-insensitive comparisons can be made */
        textcopy = (char *) malloc((textlen+1)*sizeof(char));
        strcpy(textcopy,text);
        textcopy[textlen] = '\0';
        for(c=textcopy; *c != '\0'; c++) {
            *c = tolower(*c);
        }

    }

    /* Move through the subtopics on this level looking for a match to text.
     * If one is found, return it. */
    while(subtopic) {

        /* Get a handle on the subtopic's topic string */
        topic = subtopic -> topic;

        /* Move the subtopic pointer on to the next subtopic in the list */
        subtopic = subtopic -> next;

        /* Create a lower case copy of the subtopic's topic */
        topiclen = strlen(topic);
        topiccopy = (char *) malloc((topiclen+1)*sizeof(char));
        strcpy(topiccopy,topic);
        topiccopy[topiclen] = '\0';
        for(c=topiccopy; *c != '\0'; c++) {
            *c = tolower(*c);
        }

        /* Compare the lower-case copies of the subtopic topic and the text. 
         * If they match, make a copy of the subtopic topic and return it. */
        if(strncmp(topiccopy,textcopy,textlen) == 0) {

            /* The lower-case copy of the subtopic topic is not needed 
             * any more */
            free(topiccopy);

            /* Create an exact copy of the original subtopic topic */
            dmystr = (char *) malloc((topiclen+1)*sizeof(char));
            strcpy(dmystr,topic);
            dmystr[topiclen] = '\0';

            return dmystr;
        }
        /* If no match, then free the lower-case copy of the subtopic topic */
        free(topiccopy);
    }

    /* We've searched the whole subtopic list, so free the lower-case text
     * copy, and tell readline we're done. */
    free(textcopy);
    return ((char *)NULL);
}

#endif


/*
 * Moves through readline's command line buffer reading topics.  For each
 * topic, the corresponding topicnode is located and the subtopic pointer
 * argument is pointed to it.  If a matching topicnode cannot be found
 * the subtopic argument is returned as NULL
 */

#ifdef unix

void reset_subtopic(subtopic)
Topicnode **subtopic;
{

    char *clbuf_beg, *clbuf_end;
    char *spc;
    int topiclen;

    clbuf_beg = rl_line_buffer;   /*readline cl buffer */
    clbuf_end = (char *) (rl_line_buffer + rl_point - 1);

    /* Move clbuf_beg to first word on cl */
    while(isspace(*clbuf_beg)) {
        clbuf_beg++;
    }

    /* Move clbuf_end to end of last complete word (Last word on line is topic
     * to complete */
    while(!isspace(*clbuf_end) && (clbuf_end > clbuf_beg)) {
        clbuf_end--;
    }

    while(isspace(*clbuf_end)) {
        clbuf_end--;
    }

    /* Repoint subtopic for every topic on the cl */
    while(clbuf_beg < clbuf_end) {
        spc = strchr(clbuf_beg,' ');
        topiclen = (int) (spc - clbuf_beg);

        /* Find topic node which matches cl topic and reassign
         * subtopic variable */
        for(;*subtopic;*subtopic=((*subtopic)->next)){
            if(strncmp(clbuf_beg,(*subtopic)->topic,topiclen) == 0) {
                *subtopic = ((*subtopic) -> subtopic);
                break;
            }
        }

        /* If no subtopic matched, return NULL i.e. end-of-list */
        if(!*subtopic) {
            *subtopic = (Topicnode *)NULL;
            return;
        }

        /* Move clbuf_beg to beginning of next word */
        while(!isspace(*clbuf_beg)) {
          clbuf_beg++;
        }
        while(isspace(*clbuf_beg)) {
          clbuf_beg++;
        }
    }
}

#endif
