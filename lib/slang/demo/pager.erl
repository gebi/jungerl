%%%----------------------------------------------------------------------
%%% File    : pager.erl
%%% Author  : Claes Wikstrom <klacke@kaja.hemma.net>
%%% Purpose : 
%%% Created :  1 Dec 2000 by Claes Wikstrom <klacke@kaja.hemma.net>
%%%----------------------------------------------------------------------

-module(pager).
-author('klacke@kaja.hemma.net').

-compile(export_all).

-include ("slang.hrl").

-record(file_line, {next,
		    prev,
		    data}).





demolib_exit (Signal) ->

    slang:reset_tty (),
    slang:smg_reset_smg (),
    
    if
	Signal ==  0 ->
	    halt();
	true ->
	    io:format("Exiting on signal ~p\n", [Signal]),
	    halt()
    end.


sigint_handler (Signal) ->
    demolib_exit (Signal).



% static void init_signals (void)
% {
% #ifdef SIGTSTP
%    SLsignal (SIGTSTP, sigtstp_handler);
% #endif
% #ifdef SIGINT
%    SLsignal (SIGINT, sigint_handler);
% #endif
% }

exit_error_hook (Fmt, Args) ->
    
    slang:reset_tty (),
    slang:reset_smg (),
    
    io:format(Fmt, Args),
    io:nl(),
    halt().




demolib_init_terminal () ->
    
    %% SLang_Exit_Error_Hook = exit_error_hook
						% It is wise to block the occurance of display  related 
    %signals while we are 
    %initializing. 


						%SLsig_block_signals (),
    
    slang:tt_get_terminfo (),
   
    %% SLkp_init assumes that SLtt_get_terminfo has been called. 
    
    case slang:kp_init() of
	-1 ->
	    -1;
	Ret ->
	    
	    slang:init_tty (-1, 0, 1),
	    %slang:tty_set_suspend_state (1),
	    
	    case slang:smg_init_smg () of
		-1 ->
		    -1;
		_ ->
		    0
	    end
    end.


main() ->
    File="tmp/test",
    



int main (int argc, char **argv)
{  
   if (argc == 2)
     {
	File_Name = argv[1];
     }
   else if ((argc != 1) || (1 == isatty (fileno(stdin))))
     usage (argv[0]);

   
   if (-1 == read_file (File_Name))
     {
	fprintf (stderr, "Unable to read %s\n", File_Name);
	return 1;
     }
   
   /* This sets up the terminal, signals, screen management routines, etc... */
   if (-1 == demolib_init_terminal (1, 1))
     {
	fprintf (stderr, "Unable to initialize terminal.");
	return 1;
     }
   
#define APP_KEY_EOB  0x1001
#define APP_KEY_BOB  0x1002
   
   /* Add a few application defined keysyms.  0x1000 and above are for 
    * applications.
    */
   (void) SLkp_define_keysym ("\033>", APP_KEY_EOB);
   (void) SLkp_define_keysym ("\033<", APP_KEY_BOB);
   
   main_loop ();		       /* should not return */
   return 1;
}

   
/* The SLscroll routines will be used for pageup/down commands.  They assume
 * a linked list of lines.  The first element of the structure MUST point to 
 * the NEXT line, the second MUST point to the PREVIOUS line.
 */
typedef struct _File_Line_Type
{
   struct _File_Line_Type *next;
   struct _File_Line_Type *prev;
   char *data;			       /* pointer to line data */
}
File_Line_Type;

static File_Line_Type *File_Lines;

/* The SLscroll routines will use this structure. */
static SLscroll_Window_Type Line_Window;

static void free_lines (void)
{
   File_Line_Type *line, *next;
   
   line = File_Lines;
   while (line != NULL)
     {
	next = line->next;
	if (line->data != NULL) free (line->data);
	free (line);
	line = next;
     }
   File_Lines = NULL;
}

static File_Line_Type *create_line (char *buf)
{
   File_Line_Type *line;
   
   line = (File_Line_Type *) malloc (sizeof (File_Line_Type));
   if (line == NULL) return NULL;
   
   memset ((char *) line, sizeof (File_Line_Type), 0);
   
   line->data = SLmake_string (buf);   /* use a slang routine */
   if (line->data == NULL)
     {
	free (line);
	return NULL;
     }
   
   return line;
}


static int read_file (char *file)
{
   FILE *fp;
   char buf [1024];
   File_Line_Type *line, *last_line;
   unsigned int num_lines;
   
   if (file == NULL) 
     fp = stdin;
   else fp = fopen (file, "r");
   
   if (fp == NULL) return -1;
   
   last_line = NULL;
   num_lines = 0;
   
   while (NULL != fgets (buf, sizeof(buf), fp))
     {
	num_lines++;
	
	if (NULL == (line = create_line (buf)))
	  {
	     fprintf (stderr, "Out of memory.");
	     free_lines ();
	     return -1;
	  }
	
	if (last_line == NULL)
	  File_Lines = line;
	else 
	  last_line->next = line;
	
	line->prev = last_line;
	line->next = NULL;
	
	last_line = line;
     }
   
   memset ((char *)&Line_Window, 0, sizeof (SLscroll_Window_Type));
   
   Line_Window.current_line = (SLscroll_Type *) File_Lines;
   Line_Window.lines = (SLscroll_Type *) File_Lines;
   Line_Window.line_num = 1;
   Line_Window.num_lines = num_lines;
   
   return 0;
}


static void update_display (void)
{
   unsigned int row, nrows;
   File_Line_Type *line;

   /* All well behaved applications should block signals that may affect
    * the display while performing screen update.
    */
   SLsig_block_signals ();
   
   Line_Window.nrows = nrows = SLtt_Screen_Rows - 1;

   /* Always make the current line equal to the top window line. */
   if (Line_Window.top_window_line != NULL)
     Line_Window.current_line = Line_Window.top_window_line;

   SLscroll_find_top (&Line_Window);
   
   row = 0;
   line = (File_Line_Type *) Line_Window.top_window_line;
   
   SLsmg_normal_video ();
   
   while (row < Line_Window.nrows)
     {
	SLsmg_gotorc (row, 0);
	
	if (line != NULL) 
	  {
	     SLsmg_write_string (line->data);
	     line = line->next;
	  }
	SLsmg_erase_eol ();
	row++;
     }
   
   SLsmg_gotorc (row, 0);
   SLsmg_reverse_video ();
   SLsmg_printf ("%s", (File_Name == NULL) ? "<stdin>" : File_Name);
   SLsmg_erase_eol ();
   SLsmg_refresh ();
   
   SLsig_unblock_signals ();
}

static int Screen_Start;

static void main_loop (void)
{
   int screen_start;

   while (1)
     {
	update_display ();
	switch (SLkp_getkey ())
	  {
	   case SL_KEY_ERR:
	   case 'q':
	   case 'Q':
	     demolib_exit (0);
	     break;
	     
	   case SL_KEY_RIGHT:
	     Screen_Start += 1;
	     screen_start = Screen_Start;
	     SLsmg_set_screen_start (NULL, &screen_start);
	     break;
	     
	   case SL_KEY_LEFT:
	     Screen_Start -= 1;
	     if (Screen_Start < 0) Screen_Start = 0;
	     screen_start = Screen_Start;
	     SLsmg_set_screen_start (NULL, &screen_start);
	     break;

	   case SL_KEY_UP:
	     SLscroll_prev_n (&Line_Window, 1);
	     Line_Window.top_window_line = Line_Window.current_line;
	     break;
	     
	   case '\r':
	   case SL_KEY_DOWN:
	     SLscroll_next_n (&Line_Window, 1);
	     Line_Window.top_window_line = Line_Window.current_line;
	     break;
	     
	   case SL_KEY_NPAGE:
	   case ' ': case 4:
	     SLscroll_pagedown (&Line_Window);
	     break;

	   case SL_KEY_PPAGE:
	   case 127: case 21:
	     SLscroll_pageup (&Line_Window);
	     break;

	   case APP_KEY_BOB:
	     while (-1 != SLscroll_pageup (&Line_Window))
	       ;
	     break;
	     
	   case APP_KEY_EOB:
	     while (-1 != SLscroll_pagedown (&Line_Window))
	       ;
	     break;
	     
	   default:
	     SLtt_beep ();
	  }
     }
}


