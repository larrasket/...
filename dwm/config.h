static unsigned int borderpx = 1; /* border pixel of windows */
static unsigned int snap = 4;     /* snap pixel */
static const int swallowfloating =
    0; /* 1 means swallow floating windows by default */
#include <X11/XF86keysym.h>
static unsigned int gappx = 20;   /* pixel gap between clients */
static const int showbar = 1;     /* 0 means no bar */
static const int topbar = 1;      /* 0 means bottom bar */
static const int horizpadbar = 6; /* horizontal padding for statusbar */
static const int vertpadbar = 7;  /* vertical padding for statusbar */
static const int lockfullscreen =
    0; /* 1 will force focus on the fullscreen window */
// static const int nmaster     = 1;    /* number of clients in master area */
//
// static const int resizehints = 1;    /* 1 means respect size hints in tiled
// resizals */
static const char *pctlpreviouscmd[] = {"playerctl", "previous", NULL};
static const char *pctlplaycmd[] = {"playerctl", "play-pause", NULL};
static const char *pctlpausecmd[] = {"playerctl", "pause", NULL};
static const char *pctlnextcmd[] = {"playerctl", "next", NULL};
static const char *gbrowsercmd[] = {"brave", NULL};
static const char *ggm[] = {"keepassxc", NULL};
static const char *exp[] = {"thunar", NULL};
static const char *ranger[] = {"kitty", "-e", "ranger", NULL};
static const char *mov[] = {
    "brave", "https://www.notion.so/Home-9061ccbeaa414fb0b7335c42f8299692",
    NULL};
static const char *scr[] = {"flameshot", "gui", NULL};
static const char *mail[] = {"mail", NULL};
static const char *pdf[] = {"zathura", NULL};
static const char *copy[] = {"rofi", "-show", "drun", "-show-icons", NULL};
static const char *copy1[] = {"rofi", "-show", "emoji", "-modi", "emoji", NULL};
static const char *lf[] = {"kitty", "-e", "lfrun", NULL};
static const char *tel[] = {"caprine", NULL};
static const char *dis[] = {"brave", "discord.com", NULL};
static const char *ms[] = {"brave", "m.me", NULL};
static const char *fid[] = { "brave", "chrome-extension://mefgmmbdailogpfhfblcnnjfmnpnmdfa/reader.html", NULL};
static const char *htop[] = {"kitty", "-e ", "htop", NULL};
static const char *yakyak[] = {"yakyak", NULL};
static const char *pad[] = {"leafpad", NULL};
static const char *start[] = {"kitty", "-e ", "rsblocks", "&", "dunst", NULL};
static const char *trello[] = {"brave", "https://www.google.com ", NULL};
//#include "fibonacci.h"
static const char *brightcmd[][4] = {
    {"light", "-A", "5", NULL},
    {"light", "-U", "5", NULL},

};

static const char *upvol[] = {"/usr/bin/pactl", "set-sink-volume",
                              "@DEFAULT_SINK@", "+5%", NULL};
static const char *downvol[] = {"/usr/bin/pactl", "set-sink-volume",
                                "@DEFAULT_SINK@", "-5%", NULL};
static const char *mutevol[] = {"/usr/bin/pactl", "set-sink-mute",
                                "@DEFAULT_SINK@", "toggle", NULL};

/* Mononoki Nerd Font must be installed from AUR nerd-fonts-complete.
 * Otherwise, your default font will be Hack which is found in the standard
 * Arch repos and is listed as a dependency for this build. JoyPixels is also
 * a hard dependency and makes colored fonts and emojis possible.
 */

/*static const char *fonts[] = {
      "Iosevka Term:size=8.5:antialias=true:autohint=true",
      "Hack:size=8.5:antialias=true:autohint=true",
    "JoyPixels:size=8.5:antialias=true:autohint=true"};*/

static const char *fonts[] = {
    "Mononoki Nerd Font:size=9.5:antialias=true:autohint=true",
    "Hack:size=9.5:antialias=true:autohint=true",
    "JoyPixels:size=9.5:antialias=true:autohint=true"};

/*static const char col_gray1[] = "#151515";
static const char col_gray2[] = "#444444";
static const char col_gray3[] = "#bbbbbb";
static const char col_gray4[] = "#eeeeee";
static char col_cyan[] = "#005577";*/

static const char col_gray1[] = "#F8F8F8";
static const char col_gray2[] = "#888580";
static const char col_gray3[] = "#1C1C1C";
static const char col_gray4[] = "#1C1C1C";
static const char col_cyan[] = "#D4D0C8";
/*col_cyan = ";*/
/* bar opacity
 * 0xff is no transparency.
 * 0xee adds wee bit of transparency.
 * 0xdd adds adds a bit more transparency.
 * Play with the value to get desired transparency.
 */
static const unsigned int baralpha = 0xff;
static const unsigned int borderalpha = OPAQUE;

// static const char *colors[][3]        = {
/*               fg         bg         border   */
//	[SchemeNorm] = { col_3, col_1, col_2 },
//	[SchemeSel]  = { col_3, col_4, col_4 },
//};

// static const char *colors[][3] = {
//     /*               fg         bg         border   */
//     [SchemeNorm] = {col_gray3, col_gray1, col_gray2},
//     [SchemeSel] = {col_gray4, col_gray1, col_cyan},
// };
static const char *colors[][3] = {
    /*               fg         bg         border   */
    [SchemeNorm] = {col_gray3, col_gray1, col_gray2},
    [SchemeSel] = {col_gray4, col_cyan, col_cyan},
};

static const unsigned int alphas[][3] = {
    /*               fg      bg        border     */
    [SchemeNorm] = {OPAQUE, baralpha, borderalpha},
    [SchemeSel] = {OPAQUE, baralpha, borderalpha},
};

/* static const char *tags[] = { "www", "read", "dev", "work", "chat", "file",
 * "movie", "type", "music" };  */
/* static const char *tags[] = { "dev", "www", "work", "read", "chat", "file",
 * "movie", "type", "music" }; */
/* tagging */
static const char *tags[] = {"1", "2", "3", "4", "5", "6", "7", "8", "9"};

// static const char *tags[] = { "", "", "", "", "", "", "", "", "" };

static const Rule rules[] = {
    /* xprop(1):
     *	WM_CLASS(STRING) = instance, class
     *	WM_NAME(STRING) = title
     */
    /* class     instance  title           tags mask  isfloating  isterminal noswallow  monitor */
    {"Gimp", NULL, NULL, 0, 1, 0, 0, -1},
    {"*Emacs Anywhere*", NULL, NULL, 0, 1, 0, 0, -1},
    /* { "Firefox", NULL,     NULL,           1 << 8,    0,          0, -1,
       -1 }, */
    {"St", NULL, NULL, 0, 0, 1, 0, -1},
    {"kitty", NULL, NULL, 0, 0, 1, 0, -1},
    {NULL, NULL, "Event Tester", 0, 0, 0, 1, -1}, /* xev */
};

/* layout(s) */
static const float mfact = 0.50; /* factor of master area size [0.05..0.95] */
static const int nmaster = 1;    /* number of clients in master area */
static const int resizehints =
    1; /* 1 means respect size hints in tiled resizals */

#include "layouts.c"
static const Layout layouts[] = {
    /* symbol     arrange function */
    {"[]=", col},
    {"HHH", monocle},
    {"[]=", tile}, /* first entry is default */
    {"[M]", monocle},
    {"><>", NULL}, /* no layout function means floating behavior */
    {NULL, NULL},
};

/* key definitions */
#define MODKEY Mod4Mask
#define TAGKEYS(CHAIN, KEY, TAG)                                               \
  {MODKEY, CHAIN, KEY, view, {.ui = 1 << TAG}},                                \
      {MODKEY | ControlMask, CHAIN, KEY, toggleview, {.ui = 1 << TAG}},        \
      {MODKEY | ShiftMask, CHAIN, KEY, tag, {.ui = 1 << TAG}},                 \
      {MODKEY | ControlMask | ShiftMask,                                       \
       CHAIN,                                                                  \
       KEY,                                                                    \
       toggletag,                                                              \
       {.ui = 1 << TAG}},
#define CMD(cmd)                                                               \
  {                                                                            \
    .v = (const char *[]) { "/bin/sh", "-c", cmd, NULL }                       \
  }

/* helper for spawning shell commands in the pre dwm-5.0 fashion */
#define SHCMD(cmd)                                                             \
  {                                                                            \
    .v = (const char *[]) { "/bin/sh", "-c", cmd, NULL }                       \
  }

/* dmenu */
static char dmenumon[2] =
    "0"; /* component of dmenucmd, manipulated in spawn() */
         /* If you are using the standard dmenu program, use the following. */
/*static const char *dmenucmd[]    = { "dmenu_run", "-p", "Run: ", NULL };*/
/* If you are using the dmenu-distrotube-git program, use the following for a
 * cooler dmenu! */
static const char *dmenucmd[] = {"dmenu_run", "-g", "10",    "-l",
                                 "48",        "-p", "Run: ", NULL};
static const char *emacs[] = {"emaks", NULL};
static const char *anyywhere[] = {"/home/ghd/.emacs_anywhere/bin/run", NULL};
/* the st terminal with tabbed */
/*static const char *termcmd[]  = { "kitty", "-e" , "bicon",  NULL };*/
static const char *termcmd[] = {"kitty", "-e", "bash", NULL};
/* An alternative way to launch st along with the fish shell */
/* static const char *termcmd[]     = { "kitty", "-e fish", NULL }; */
static const char *tabtermcmd[] = {"tabbed", "-r", "2", "kitty",
                                   "-w",     "''", NULL};
void gapm(int) {
  gappx = 0;
  borderpx = 0;
  snap = 0;
  strcpy(col_cyan, " #f0b050");
  /*	col_cyan= "";*/
}
void gapl(int) {
  gappx = 20;
  borderpx = 1;
  snap = 4;
  /*	snap=32;*/
  /*	borderpx=2;*/
}

static Key keys[] = {
    /* modifier             chain key  key        function        argument */

    {MODKEY, -1, XK_p, spawn, {.v = dmenucmd}},
    {MODKEY, -1, XK_o, spawn, {.v = copy}},
    {MODKEY, -1, XK_Return, spawn, {.v = termcmd}},
    {MODKEY | ShiftMask, -1, XK_j, rotatestack, {.i = +1}},
    {MODKEY | ShiftMask, -1, XK_k, rotatestack, {.i = -1}},
    {MODKEY, -1, XK_j, focusstack, {.i = +1}},
    {MODKEY, -1, XK_k, focusstack, {.i = -1}},
    {MODKEY, -1, XK_i, incnmaster, {.i = +1}},
    {MODKEY, -1, XK_d, incnmaster, {.i = -1}},
    {MODKEY, -1, XK_h, setmfact, {.f = -0.05}},
    {MODKEY, -1, XK_l, setmfact, {.f = +0.05}},
    {MODKEY | ControlMask, -1, XK_Return, zoom, {0}},
    {MODKEY, -1, XK_Tab, view, {0}},
    {MODKEY, -1, XK_c, killclient, {0}},

    {MODKEY | ShiftMask, -1, XK_s, spawn, {.v = scr}},
    {MODKEY | ShiftMask, -1, XK_y, spawn, {.v = start}},
    {MODKEY, -1, XK_e, spawn, {.v = lf}},
    {MODKEY | ShiftMask, -1, XK_d, spawn, {.v = dis}},
    {MODKEY | ShiftMask, -1, XK_x, spawn, {.v = htop}},
    {MODKEY | ShiftMask, -1, XK_r, spawn, {.v = ranger}},
    {MODKEY | ShiftMask, -1, XK_f, spawn, {.v = fid}},
    {MODKEY, -1, XK_w, spawn, {.v = emacs}},
    {MODKEY | ShiftMask, -1, XK_w, spawn, {.v =  anyywhere}},
    {MODKEY, XK_s, -1, setlayout, {.v = &layouts[4]}},
    {0, -1, XF86XK_AudioPlay, spawn, {.v = pctlplaycmd}},
    {0, -1, XF86XK_AudioPause, spawn, {.v = pctlpausecmd}},
    {0, -1, XF86XK_AudioNext, spawn, {.v = pctlnextcmd}},
    {0, -1, XF86XK_AudioPrev, spawn, {.v = pctlpreviouscmd}},
    {MODKEY | ShiftMask, -1, XK_n, spawn, {.v = mov}},
    {MODKEY | ShiftMask, -1, XK_l, spawn, {.v = pad}},
    {MODKEY, -1, XK_z, spawn, {.v = pdf}},
    {MODKEY | ShiftMask, -1, XK_y, spawn, {.v = yakyak}},
    {MODKEY | ShiftMask, -1, XK_t, spawn, {.v = tel}},
    {MODKEY | ShiftMask, -1, XK_g, spawn, {.v = trello}},
    {MODKEY, -1, XK_r, setlayout, {.v = &layouts[4]}},
    {MODKEY | ShiftMask, -1, XK_c, spawn, {.v = gbrowsercmd}},
    {MODKEY | ShiftMask, -1, XK_m, spawn, {.v = ms}},
    {MODKEY | ShiftMask, -1, XK_p, spawn, {.v = ggm}},
    {MODKEY | ShiftMask, -1, XK_e, spawn, {.v = exp}},
    {MODKEY, -1, XK_semicolon, spawn, {.v = copy1}},
    {0, -1, XF86XK_AudioMute, spawn, {.v = mutevol}},
    {0, -1, XF86XK_AudioLowerVolume, spawn, {.v = downvol}},
    {0, -1, XF86XK_AudioRaiseVolume, spawn, {.v = upvol}},
    {0,
     -1,
     XF86XK_MonBrightnessUp,
     spawn,
     {.v = brightcmd[0]}}, //                    brightness up
    {0, -1, XF86XK_MonBrightnessDown, spawn, {.v = brightcmd[1]}},
    {MODKEY, -1, XK_b, togglebar, {0}},

    /* Layout manipulation */
    /* { MODKEY,               -1,        XK_Tab,    cyclelayout,    {.i = -1 }
       }, */
    /* { MODKEY|ShiftMask,     -1,        XK_Tab,    cyclelayout,    {.i = +1 }
       }, */
    {MODKEY, -1, XK_space, setlayout, {0}},
    {MODKEY | ShiftMask, -1, XK_space, togglefloating, {0}},
    {MODKEY, -1, XK_x, gapm, {0}},
    {MODKEY, -1, XK_a, gapl, {0}},

    /* Switch to specific layouts */
    {MODKEY, -1, XK_t, setlayout, {.v = &layouts[0]}},
    {MODKEY, -1, XK_f, setlayout, {.v = &layouts[1]}},
    {MODKEY, -1, XK_m, setlayout, {.v = &layouts[2]}},
    {MODKEY, -1, XK_g, setlayout, {.v = &layouts[3]}},

    {MODKEY, -1, XK_0, view, {.ui = ~0}},
    {MODKEY | ShiftMask, -1, XK_0, tag, {.ui = ~0}},

    /* Switching between monitors */
    {MODKEY, -1, XK_comma, focusmon, {.i = -1}},
    {MODKEY, -1, XK_period, focusmon, {.i = +1}},
    {MODKEY | ShiftMask, -1, XK_comma, tagmon, {.i = -1}},
    {MODKEY | ShiftMask, -1, XK_period, tagmon, {.i = +1}},

    TAGKEYS(-1, XK_1, 0) TAGKEYS(-1, XK_2, 1) TAGKEYS(-1, XK_3, 2)
        TAGKEYS(-1, XK_4, 3) TAGKEYS(-1, XK_5, 4) TAGKEYS(-1, XK_6, 5)
            TAGKEYS(-1, XK_7, 6) TAGKEYS(-1, XK_8, 7)
                TAGKEYS(-1, XK_9, 8){MODKEY | ShiftMask, -1, XK_q, quit, {0}},
    {MODKEY | ShiftMask, -1, XK_r, quit, {1}},
};

static Button buttons[] = {
    /* click           event mask   button          function        argument */
    {ClkLtSymbol, 0, Button1, setlayout, {0}},
    {ClkLtSymbol, 0, Button3, setlayout, {.v = &layouts[2]}},
    {ClkWinTitle, 0, Button2, zoom, {0}},
    {ClkStatusText, 0, Button2, spawn, {.v = termcmd}},
    {ClkClientWin, MODKEY, Button1, movemouse, {0}},
    {ClkClientWin, MODKEY, Button2, togglefloating, {0}},
    {ClkClientWin, MODKEY, Button3, resizemouse, {0}},
    {ClkTagBar, 0, Button1, view, {0}},
    {ClkTagBar, 0, Button3, toggleview, {0}},
    {ClkTagBar, MODKEY, Button1, tag, {0}},
    {ClkTagBar, MODKEY, Button3, toggletag, {0}},
};

