--TEST--
Translation of HTML entities for encoding KOI8-R
--FILE--
<?php
$arr = array(
0x2500 => array(0x80, "BOX DRAWINGS LIGHT HORIZONTAL"),
0x2502 => array(0x81, "BOX DRAWINGS LIGHT VERTICAL"),
0x250C => array(0x82, "BOX DRAWINGS LIGHT DOWN AND RIGHT"),
0x2510 => array(0x83, "BOX DRAWINGS LIGHT DOWN AND LEFT"),
0x2514 => array(0x84, "BOX DRAWINGS LIGHT UP AND RIGHT"),
0x2518 => array(0x85, "BOX DRAWINGS LIGHT UP AND LEFT"),
0x251C => array(0x86, "BOX DRAWINGS LIGHT VERTICAL AND RIGHT"),
0x2524 => array(0x87, "BOX DRAWINGS LIGHT VERTICAL AND LEFT"),
0x252C => array(0x88, "BOX DRAWINGS LIGHT DOWN AND HORIZONTAL"),
0x2534 => array(0x89, "BOX DRAWINGS LIGHT UP AND HORIZONTAL"),
0x253C => array(0x8A, "BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL"),
0x2580 => array(0x8B, "UPPER HALF BLOCK"),
0x2584 => array(0x8C, "LOWER HALF BLOCK"),
0x2588 => array(0x8D, "FULL BLOCK"),
0x258C => array(0x8E, "LEFT HALF BLOCK"),
0x2590 => array(0x8F, "RIGHT HALF BLOCK"),
0x2591 => array(0x90, "LIGHT SHADE"),
0x2592 => array(0x91, "MEDIUM SHADE"),
0x2593 => array(0x92, "DARK SHADE"),
0x2320 => array(0x93, "TOP HALF INTEGRAL"),
0x25A0 => array(0x94, "BLACK SQUARE"),
0x2219 => array(0x95, "BULLET OPERATOR"),
0x221A => array(0x96, "SQUARE ROOT"),
0x2248 => array(0x97, "ALMOST EQUAL TO"),
0x2264 => array(0x98, "LESS-THAN OR EQUAL TO"),
0x2265 => array(0x99, "GREATER-THAN OR EQUAL TO"),
0x00A0 => array(0x9A, "NO-BREAK SPACE"),
0x2321 => array(0x9B, "BOTTOM HALF INTEGRAL"),
0x00B0 => array(0x9C, "DEGREE SIGN"),
0x00B2 => array(0x9D, "SUPERSCRIPT TWO"),
0x00B7 => array(0x9E, "MIDDLE DOT"),
0x00F7 => array(0x9F, "DIVISION SIGN"),
0x2550 => array(0xA0, "BOX DRAWINGS DOUBLE HORIZONTAL"),
0x2551 => array(0xA1, "BOX DRAWINGS DOUBLE VERTICAL"),
0x2552 => array(0xA2, "BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE"),
0x0451 => array(0xA3, "CYRILLIC SMALL LETTER IO"),
0x2553 => array(0xA4, "BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE"),
0x2554 => array(0xA5, "BOX DRAWINGS DOUBLE DOWN AND RIGHT"),
0x2555 => array(0xA6, "BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE"),
0x2556 => array(0xA7, "BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE"),
0x2557 => array(0xA8, "BOX DRAWINGS DOUBLE DOWN AND LEFT"),
0x2558 => array(0xA9, "BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE"),
0x2559 => array(0xAA, "BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE"),
0x255A => array(0xAB, "BOX DRAWINGS DOUBLE UP AND RIGHT"),
0x255B => array(0xAC, "BOX DRAWINGS UP SINGLE AND LEFT DOUBLE"),
0x255C => array(0xAD, "BOX DRAWINGS UP DOUBLE AND LEFT SINGLE"),
0x255D => array(0xAE, "BOX DRAWINGS DOUBLE UP AND LEFT"),
0x255E => array(0xAF, "BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE"),
0x255F => array(0xB0, "BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE"),
0x2560 => array(0xB1, "BOX DRAWINGS DOUBLE VERTICAL AND RIGHT"),
0x2561 => array(0xB2, "BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE"),
0x0401 => array(0xB3, "CYRILLIC CAPITAL LETTER IO"),
0x2562 => array(0xB4, "BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE"),
0x2563 => array(0xB5, "BOX DRAWINGS DOUBLE VERTICAL AND LEFT"),
0x2564 => array(0xB6, "BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE"),
0x2565 => array(0xB7, "BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE"),
0x2566 => array(0xB8, "BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL"),
0x2567 => array(0xB9, "BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE"),
0x2568 => array(0xBA, "BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE"),
0x2569 => array(0xBB, "BOX DRAWINGS DOUBLE UP AND HORIZONTAL"),
0x256A => array(0xBC, "BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE"),
0x256B => array(0xBD, "BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE"),
0x256C => array(0xBE, "BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL"),
0x00A9 => array(0xBF, "COPYRIGHT SIGN"),
0x044E => array(0xC0, "CYRILLIC SMALL LETTER YU"),
0x0430 => array(0xC1, "CYRILLIC SMALL LETTER A"),
0x0431 => array(0xC2, "CYRILLIC SMALL LETTER BE"),
0x0446 => array(0xC3, "CYRILLIC SMALL LETTER TSE"),
0x0434 => array(0xC4, "CYRILLIC SMALL LETTER DE"),
0x0435 => array(0xC5, "CYRILLIC SMALL LETTER IE"),
0x0444 => array(0xC6, "CYRILLIC SMALL LETTER EF"),
0x0433 => array(0xC7, "CYRILLIC SMALL LETTER GHE"),
0x0445 => array(0xC8, "CYRILLIC SMALL LETTER HA"),
0x0438 => array(0xC9, "CYRILLIC SMALL LETTER I"),
0x0439 => array(0xCA, "CYRILLIC SMALL LETTER SHORT I"),
0x043A => array(0xCB, "CYRILLIC SMALL LETTER KA"),
0x043B => array(0xCC, "CYRILLIC SMALL LETTER EL"),
0x043C => array(0xCD, "CYRILLIC SMALL LETTER EM"),
0x043D => array(0xCE, "CYRILLIC SMALL LETTER EN"),
0x043E => array(0xCF, "CYRILLIC SMALL LETTER O"),
0x043F => array(0xD0, "CYRILLIC SMALL LETTER PE"),
0x044F => array(0xD1, "CYRILLIC SMALL LETTER YA"),
0x0440 => array(0xD2, "CYRILLIC SMALL LETTER ER"),
0x0441 => array(0xD3, "CYRILLIC SMALL LETTER ES"),
0x0442 => array(0xD4, "CYRILLIC SMALL LETTER TE"),
0x0443 => array(0xD5, "CYRILLIC SMALL LETTER U"),
0x0436 => array(0xD6, "CYRILLIC SMALL LETTER ZHE"),
0x0432 => array(0xD7, "CYRILLIC SMALL LETTER VE"),
0x044C => array(0xD8, "CYRILLIC SMALL LETTER SOFT SIGN"),
0x044B => array(0xD9, "CYRILLIC SMALL LETTER YERU"),
0x0437 => array(0xDA, "CYRILLIC SMALL LETTER ZE"),
0x0448 => array(0xDB, "CYRILLIC SMALL LETTER SHA"),
0x044D => array(0xDC, "CYRILLIC SMALL LETTER E"),
0x0449 => array(0xDD, "CYRILLIC SMALL LETTER SHCHA"),
0x0447 => array(0xDE, "CYRILLIC SMALL LETTER CHE"),
0x044A => array(0xDF, "CYRILLIC SMALL LETTER HARD SIGN"),
0x042E => array(0xE0, "CYRILLIC CAPITAL LETTER YU"),
0x0410 => array(0xE1, "CYRILLIC CAPITAL LETTER A"),
0x0411 => array(0xE2, "CYRILLIC CAPITAL LETTER BE"),
0x0426 => array(0xE3, "CYRILLIC CAPITAL LETTER TSE"),
0x0414 => array(0xE4, "CYRILLIC CAPITAL LETTER DE"),
0x0415 => array(0xE5, "CYRILLIC CAPITAL LETTER IE"),
0x0424 => array(0xE6, "CYRILLIC CAPITAL LETTER EF"),
0x0413 => array(0xE7, "CYRILLIC CAPITAL LETTER GHE"),
0x0425 => array(0xE8, "CYRILLIC CAPITAL LETTER HA"),
0x0418 => array(0xE9, "CYRILLIC CAPITAL LETTER I"),
0x0419 => array(0xEA, "CYRILLIC CAPITAL LETTER SHORT I"),
0x041A => array(0xEB, "CYRILLIC CAPITAL LETTER KA"),
0x041B => array(0xEC, "CYRILLIC CAPITAL LETTER EL"),
0x041C => array(0xED, "CYRILLIC CAPITAL LETTER EM"),
0x041D => array(0xEE, "CYRILLIC CAPITAL LETTER EN"),
0x041E => array(0xEF, "CYRILLIC CAPITAL LETTER O"),
0x041F => array(0xF0, "CYRILLIC CAPITAL LETTER PE"),
0x042F => array(0xF1, "CYRILLIC CAPITAL LETTER YA"),
0x0420 => array(0xF2, "CYRILLIC CAPITAL LETTER ER"),
0x0421 => array(0xF3, "CYRILLIC CAPITAL LETTER ES"),
0x0422 => array(0xF4, "CYRILLIC CAPITAL LETTER TE"),
0x0423 => array(0xF5, "CYRILLIC CAPITAL LETTER U"),
0x0416 => array(0xF6, "CYRILLIC CAPITAL LETTER ZHE"),
0x0412 => array(0xF7, "CYRILLIC CAPITAL LETTER VE"),
0x042C => array(0xF8, "CYRILLIC CAPITAL LETTER SOFT SIGN"),
0x042B => array(0xF9, "CYRILLIC CAPITAL LETTER YERU"),
0x0417 => array(0xFA, "CYRILLIC CAPITAL LETTER ZE"),
0x0428 => array(0xFB, "CYRILLIC CAPITAL LETTER SHA"),
0x042D => array(0xFC, "CYRILLIC CAPITAL LETTER E"),
0x0429 => array(0xFD, "CYRILLIC CAPITAL LETTER SHCHA"),
0x0427 => array(0xFE, "CYRILLIC CAPITAL LETTER CHE"),
0x042A => array(0xFF, "CYRILLIC CAPITAL LETTER HARD SIGN"),
);

foreach ($arr as $u => $v) {
    $ent = sprintf("&#x%X;", $u);
    $res = html_entity_decode($ent, ENT_QUOTES, 'KOI8-R');
    $d = unpack("H*", $res);
    echo sprintf("%s: %s => %s\n", $v[1], $ent, $d[1]);
    
    $ent = sprintf("&#x%X;", $v[0]);
    $res = html_entity_decode($ent, ENT_QUOTES, 'KOI8-R');
    if ($res[0] != "&" || $res[1] != "#")
        $res = unpack("H*", $res)[1];
    echo sprintf("%s => %s\n\n", $ent, $res);
}
?>
BOX DRAWINGS LIGHT HORIZONTAL: &#x2500; => 80
&#x80; => &#x80;

BOX DRAWINGS LIGHT VERTICAL: &#x2502; => 81
&#x81; => &#x81;

BOX DRAWINGS LIGHT DOWN AND RIGHT: &#x250C; => 82
&#x82; => &#x82;

BOX DRAWINGS LIGHT DOWN AND LEFT: &#x2510; => 83
&#x83; => &#x83;

BOX DRAWINGS LIGHT UP AND RIGHT: &#x2514; => 84
&#x84; => &#x84;

BOX DRAWINGS LIGHT UP AND LEFT: &#x2518; => 85
&#x85; => &#x85;

BOX DRAWINGS LIGHT VERTICAL AND RIGHT: &#x251C; => 86
&#x86; => &#x86;

BOX DRAWINGS LIGHT VERTICAL AND LEFT: &#x2524; => 87
&#x87; => &#x87;

BOX DRAWINGS LIGHT DOWN AND HORIZONTAL: &#x252C; => 88
&#x88; => &#x88;

BOX DRAWINGS LIGHT UP AND HORIZONTAL: &#x2534; => 89
&#x89; => &#x89;

BOX DRAWINGS LIGHT VERTICAL AND HORIZONTAL: &#x253C; => 8a
&#x8A; => &#x8A;

UPPER HALF BLOCK: &#x2580; => 8b
&#x8B; => &#x8B;

LOWER HALF BLOCK: &#x2584; => 8c
&#x8C; => &#x8C;

FULL BLOCK: &#x2588; => 8d
&#x8D; => &#x8D;

LEFT HALF BLOCK: &#x258C; => 8e
&#x8E; => &#x8E;

RIGHT HALF BLOCK: &#x2590; => 8f
&#x8F; => &#x8F;

LIGHT SHADE: &#x2591; => 90
&#x90; => &#x90;

MEDIUM SHADE: &#x2592; => 91
&#x91; => &#x91;

DARK SHADE: &#x2593; => 92
&#x92; => &#x92;

TOP HALF INTEGRAL: &#x2320; => 93
&#x93; => &#x93;

BLACK SQUARE: &#x25A0; => 94
&#x94; => &#x94;

BULLET OPERATOR: &#x2219; => 95
&#x95; => &#x95;

SQUARE ROOT: &#x221A; => 96
&#x96; => &#x96;

ALMOST EQUAL TO: &#x2248; => 97
&#x97; => &#x97;

LESS-THAN OR EQUAL TO: &#x2264; => 98
&#x98; => &#x98;

GREATER-THAN OR EQUAL TO: &#x2265; => 99
&#x99; => &#x99;

NO-BREAK SPACE: &#xA0; => 9a
&#x9A; => &#x9A;

BOTTOM HALF INTEGRAL: &#x2321; => 9b
&#x9B; => &#x9B;

DEGREE SIGN: &#xB0; => 9c
&#x9C; => &#x9C;

SUPERSCRIPT TWO: &#xB2; => 9d
&#x9D; => &#x9D;

MIDDLE DOT: &#xB7; => 9e
&#x9E; => &#x9E;

DIVISION SIGN: &#xF7; => 9f
&#x9F; => &#x9F;

BOX DRAWINGS DOUBLE HORIZONTAL: &#x2550; => a0
&#xA0; => 9a

BOX DRAWINGS DOUBLE VERTICAL: &#x2551; => a1
&#xA1; => &#xA1;

BOX DRAWINGS DOWN SINGLE AND RIGHT DOUBLE: &#x2552; => a2
&#xA2; => &#xA2;

CYRILLIC SMALL LETTER IO: &#x451; => a3
&#xA3; => &#xA3;

BOX DRAWINGS DOWN DOUBLE AND RIGHT SINGLE: &#x2553; => a4
&#xA4; => &#xA4;

BOX DRAWINGS DOUBLE DOWN AND RIGHT: &#x2554; => a5
&#xA5; => &#xA5;

BOX DRAWINGS DOWN SINGLE AND LEFT DOUBLE: &#x2555; => a6
&#xA6; => &#xA6;

BOX DRAWINGS DOWN DOUBLE AND LEFT SINGLE: &#x2556; => a7
&#xA7; => &#xA7;

BOX DRAWINGS DOUBLE DOWN AND LEFT: &#x2557; => a8
&#xA8; => &#xA8;

BOX DRAWINGS UP SINGLE AND RIGHT DOUBLE: &#x2558; => a9
&#xA9; => bf

BOX DRAWINGS UP DOUBLE AND RIGHT SINGLE: &#x2559; => aa
&#xAA; => &#xAA;

BOX DRAWINGS DOUBLE UP AND RIGHT: &#x255A; => ab
&#xAB; => &#xAB;

BOX DRAWINGS UP SINGLE AND LEFT DOUBLE: &#x255B; => ac
&#xAC; => &#xAC;

BOX DRAWINGS UP DOUBLE AND LEFT SINGLE: &#x255C; => ad
&#xAD; => &#xAD;

BOX DRAWINGS DOUBLE UP AND LEFT: &#x255D; => ae
&#xAE; => &#xAE;

BOX DRAWINGS VERTICAL SINGLE AND RIGHT DOUBLE: &#x255E; => af
&#xAF; => &#xAF;

BOX DRAWINGS VERTICAL DOUBLE AND RIGHT SINGLE: &#x255F; => b0
&#xB0; => 9c

BOX DRAWINGS DOUBLE VERTICAL AND RIGHT: &#x2560; => b1
&#xB1; => &#xB1;

BOX DRAWINGS VERTICAL SINGLE AND LEFT DOUBLE: &#x2561; => b2
&#xB2; => 9d

CYRILLIC CAPITAL LETTER IO: &#x401; => b3
&#xB3; => &#xB3;

BOX DRAWINGS VERTICAL DOUBLE AND LEFT SINGLE: &#x2562; => b4
&#xB4; => &#xB4;

BOX DRAWINGS DOUBLE VERTICAL AND LEFT: &#x2563; => b5
&#xB5; => &#xB5;

BOX DRAWINGS DOWN SINGLE AND HORIZONTAL DOUBLE: &#x2564; => b6
&#xB6; => &#xB6;

BOX DRAWINGS DOWN DOUBLE AND HORIZONTAL SINGLE: &#x2565; => b7
&#xB7; => 9e

BOX DRAWINGS DOUBLE DOWN AND HORIZONTAL: &#x2566; => b8
&#xB8; => &#xB8;

BOX DRAWINGS UP SINGLE AND HORIZONTAL DOUBLE: &#x2567; => b9
&#xB9; => &#xB9;

BOX DRAWINGS UP DOUBLE AND HORIZONTAL SINGLE: &#x2568; => ba
&#xBA; => &#xBA;

BOX DRAWINGS DOUBLE UP AND HORIZONTAL: &#x2569; => bb
&#xBB; => &#xBB;

BOX DRAWINGS VERTICAL SINGLE AND HORIZONTAL DOUBLE: &#x256A; => bc
&#xBC; => &#xBC;

BOX DRAWINGS VERTICAL DOUBLE AND HORIZONTAL SINGLE: &#x256B; => bd
&#xBD; => &#xBD;

BOX DRAWINGS DOUBLE VERTICAL AND HORIZONTAL: &#x256C; => be
&#xBE; => &#xBE;

COPYRIGHT SIGN: &#xA9; => bf
&#xBF; => &#xBF;

CYRILLIC SMALL LETTER YU: &#x44E; => c0
&#xC0; => &#xC0;

CYRILLIC SMALL LETTER A: &#x430; => c1
&#xC1; => &#xC1;

CYRILLIC SMALL LETTER BE: &#x431; => c2
&#xC2; => &#xC2;

CYRILLIC SMALL LETTER TSE: &#x446; => c3
&#xC3; => &#xC3;

CYRILLIC SMALL LETTER DE: &#x434; => c4
&#xC4; => &#xC4;

CYRILLIC SMALL LETTER IE: &#x435; => c5
&#xC5; => &#xC5;

CYRILLIC SMALL LETTER EF: &#x444; => c6
&#xC6; => &#xC6;

CYRILLIC SMALL LETTER GHE: &#x433; => c7
&#xC7; => &#xC7;

CYRILLIC SMALL LETTER HA: &#x445; => c8
&#xC8; => &#xC8;

CYRILLIC SMALL LETTER I: &#x438; => c9
&#xC9; => &#xC9;

CYRILLIC SMALL LETTER SHORT I: &#x439; => ca
&#xCA; => &#xCA;

CYRILLIC SMALL LETTER KA: &#x43A; => cb
&#xCB; => &#xCB;

CYRILLIC SMALL LETTER EL: &#x43B; => cc
&#xCC; => &#xCC;

CYRILLIC SMALL LETTER EM: &#x43C; => cd
&#xCD; => &#xCD;

CYRILLIC SMALL LETTER EN: &#x43D; => ce
&#xCE; => &#xCE;

CYRILLIC SMALL LETTER O: &#x43E; => cf
&#xCF; => &#xCF;

CYRILLIC SMALL LETTER PE: &#x43F; => d0
&#xD0; => &#xD0;

CYRILLIC SMALL LETTER YA: &#x44F; => d1
&#xD1; => &#xD1;

CYRILLIC SMALL LETTER ER: &#x440; => d2
&#xD2; => &#xD2;

CYRILLIC SMALL LETTER ES: &#x441; => d3
&#xD3; => &#xD3;

CYRILLIC SMALL LETTER TE: &#x442; => d4
&#xD4; => &#xD4;

CYRILLIC SMALL LETTER U: &#x443; => d5
&#xD5; => &#xD5;

CYRILLIC SMALL LETTER ZHE: &#x436; => d6
&#xD6; => &#xD6;

CYRILLIC SMALL LETTER VE: &#x432; => d7
&#xD7; => &#xD7;

CYRILLIC SMALL LETTER SOFT SIGN: &#x44C; => d8
&#xD8; => &#xD8;

CYRILLIC SMALL LETTER YERU: &#x44B; => d9
&#xD9; => &#xD9;

CYRILLIC SMALL LETTER ZE: &#x437; => da
&#xDA; => &#xDA;

CYRILLIC SMALL LETTER SHA: &#x448; => db
&#xDB; => &#xDB;

CYRILLIC SMALL LETTER E: &#x44D; => dc
&#xDC; => &#xDC;

CYRILLIC SMALL LETTER SHCHA: &#x449; => dd
&#xDD; => &#xDD;

CYRILLIC SMALL LETTER CHE: &#x447; => de
&#xDE; => &#xDE;

CYRILLIC SMALL LETTER HARD SIGN: &#x44A; => df
&#xDF; => &#xDF;

CYRILLIC CAPITAL LETTER YU: &#x42E; => e0
&#xE0; => &#xE0;

CYRILLIC CAPITAL LETTER A: &#x410; => e1
&#xE1; => &#xE1;

CYRILLIC CAPITAL LETTER BE: &#x411; => e2
&#xE2; => &#xE2;

CYRILLIC CAPITAL LETTER TSE: &#x426; => e3
&#xE3; => &#xE3;

CYRILLIC CAPITAL LETTER DE: &#x414; => e4
&#xE4; => &#xE4;

CYRILLIC CAPITAL LETTER IE: &#x415; => e5
&#xE5; => &#xE5;

CYRILLIC CAPITAL LETTER EF: &#x424; => e6
&#xE6; => &#xE6;

CYRILLIC CAPITAL LETTER GHE: &#x413; => e7
&#xE7; => &#xE7;

CYRILLIC CAPITAL LETTER HA: &#x425; => e8
&#xE8; => &#xE8;

CYRILLIC CAPITAL LETTER I: &#x418; => e9
&#xE9; => &#xE9;

CYRILLIC CAPITAL LETTER SHORT I: &#x419; => ea
&#xEA; => &#xEA;

CYRILLIC CAPITAL LETTER KA: &#x41A; => eb
&#xEB; => &#xEB;

CYRILLIC CAPITAL LETTER EL: &#x41B; => ec
&#xEC; => &#xEC;

CYRILLIC CAPITAL LETTER EM: &#x41C; => ed
&#xED; => &#xED;

CYRILLIC CAPITAL LETTER EN: &#x41D; => ee
&#xEE; => &#xEE;

CYRILLIC CAPITAL LETTER O: &#x41E; => ef
&#xEF; => &#xEF;

CYRILLIC CAPITAL LETTER PE: &#x41F; => f0
&#xF0; => &#xF0;

CYRILLIC CAPITAL LETTER YA: &#x42F; => f1
&#xF1; => &#xF1;

CYRILLIC CAPITAL LETTER ER: &#x420; => f2
&#xF2; => &#xF2;

CYRILLIC CAPITAL LETTER ES: &#x421; => f3
&#xF3; => &#xF3;

CYRILLIC CAPITAL LETTER TE: &#x422; => f4
&#xF4; => &#xF4;

CYRILLIC CAPITAL LETTER U: &#x423; => f5
&#xF5; => &#xF5;

CYRILLIC CAPITAL LETTER ZHE: &#x416; => f6
&#xF6; => &#xF6;

CYRILLIC CAPITAL LETTER VE: &#x412; => f7
&#xF7; => 9f

CYRILLIC CAPITAL LETTER SOFT SIGN: &#x42C; => f8
&#xF8; => &#xF8;

CYRILLIC CAPITAL LETTER YERU: &#x42B; => f9
&#xF9; => &#xF9;

CYRILLIC CAPITAL LETTER ZE: &#x417; => fa
&#xFA; => &#xFA;

CYRILLIC CAPITAL LETTER SHA: &#x428; => fb
&#xFB; => &#xFB;

CYRILLIC CAPITAL LETTER E: &#x42D; => fc
&#xFC; => &#xFC;

CYRILLIC CAPITAL LETTER SHCHA: &#x429; => fd
&#xFD; => &#xFD;

CYRILLIC CAPITAL LETTER CHE: &#x427; => fe
&#xFE; => &#xFE;

CYRILLIC CAPITAL LETTER HARD SIGN: &#x42A; => ff
&#xFF; => &#xFF;


