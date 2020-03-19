/* See LICENSE file for copyright and license details. */
/* Default settings; can be overriden by command line. */

static int topbar = 1;                      /* -b  option; if 0, dmenu appears at bottom     */
/* -fn option overrides fonts[0]; default X11 font or font set */
static const char *fonts[] = {
	"Cascadia Code:size=14"
};
static const char *prompt      = "[dmenu]";      /* -p  option; prompt to the left of input field */
static const char *colors[SchemeLast][2] = {
	/*     fg         bg       */
	[SchemeNorm] = { "#DEDEDE", "#052F3E" },
	[SchemeSel]  = { "#010101", "#50D49B" },
	[SchemeMid]  = { "#DEDEDE", "#052F3E" },
	[SchemeNormHighlight] = { "#C5A25C", "#052F3E" },
	[SchemeSelHighlight]  = { "#88703F", "#50D49B" },
	[SchemeOut]  = { "#000000", "#00ffff" },
};
/* -l option; if nonzero, dmenu uses vertical list with given number of lines */
static unsigned int lines      =14;
static unsigned int lineheight =5;
/*
 * Characters not considered part of a word while deleting words
 * for example: " /?\"&[]"
 */
static const char worddelimiters[] = " ";
static const unsigned int border_width = 1;
