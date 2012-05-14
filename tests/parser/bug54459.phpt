--TEST--
Bug #54459 (Range function accuracy)
--INI--
precision=14
--FILE--
<?php
foreach (range(90, 100, .1) as $i => $v){
  echo $i, ' = ', $v, PHP_EOL;
}
foreach (range("90", "100", .1) as $i => $v){
  echo $i, ' = ', $v, PHP_EOL;
}
?>
0 = 90
1 = 90.1
2 = 90.2
3 = 90.3
4 = 90.4
5 = 90.5
6 = 90.6
7 = 90.7
8 = 90.8
9 = 90.9
10 = 91
11 = 91.1
12 = 91.2
13 = 91.3
14 = 91.4
15 = 91.5
16 = 91.6
17 = 91.7
18 = 91.8
19 = 91.9
20 = 92
21 = 92.1
22 = 92.2
23 = 92.3
24 = 92.4
25 = 92.5
26 = 92.6
27 = 92.7
28 = 92.8
29 = 92.9
30 = 93
31 = 93.1
32 = 93.2
33 = 93.3
34 = 93.4
35 = 93.5
36 = 93.6
37 = 93.7
38 = 93.8
39 = 93.9
40 = 94
41 = 94.1
42 = 94.2
43 = 94.3
44 = 94.4
45 = 94.5
46 = 94.6
47 = 94.7
48 = 94.8
49 = 94.9
50 = 95
51 = 95.1
52 = 95.2
53 = 95.3
54 = 95.4
55 = 95.5
56 = 95.6
57 = 95.7
58 = 95.8
59 = 95.9
60 = 96
61 = 96.1
62 = 96.2
63 = 96.3
64 = 96.4
65 = 96.5
66 = 96.6
67 = 96.7
68 = 96.8
69 = 96.9
70 = 97
71 = 97.1
72 = 97.2
73 = 97.3
74 = 97.4
75 = 97.5
76 = 97.6
77 = 97.7
78 = 97.8
79 = 97.9
80 = 98
81 = 98.1
82 = 98.2
83 = 98.3
84 = 98.4
85 = 98.5
86 = 98.6
87 = 98.7
88 = 98.8
89 = 98.9
90 = 99
91 = 99.1
92 = 99.2
93 = 99.3
94 = 99.4
95 = 99.5
96 = 99.6
97 = 99.7
98 = 99.8
99 = 99.9
100 = 100
0 = 90
1 = 90.1
2 = 90.2
3 = 90.3
4 = 90.4
5 = 90.5
6 = 90.6
7 = 90.7
8 = 90.8
9 = 90.9
10 = 91
11 = 91.1
12 = 91.2
13 = 91.3
14 = 91.4
15 = 91.5
16 = 91.6
17 = 91.7
18 = 91.8
19 = 91.9
20 = 92
21 = 92.1
22 = 92.2
23 = 92.3
24 = 92.4
25 = 92.5
26 = 92.6
27 = 92.7
28 = 92.8
29 = 92.9
30 = 93
31 = 93.1
32 = 93.2
33 = 93.3
34 = 93.4
35 = 93.5
36 = 93.6
37 = 93.7
38 = 93.8
39 = 93.9
40 = 94
41 = 94.1
42 = 94.2
43 = 94.3
44 = 94.4
45 = 94.5
46 = 94.6
47 = 94.7
48 = 94.8
49 = 94.9
50 = 95
51 = 95.1
52 = 95.2
53 = 95.3
54 = 95.4
55 = 95.5
56 = 95.6
57 = 95.7
58 = 95.8
59 = 95.9
60 = 96
61 = 96.1
62 = 96.2
63 = 96.3
64 = 96.4
65 = 96.5
66 = 96.6
67 = 96.7
68 = 96.8
69 = 96.9
70 = 97
71 = 97.1
72 = 97.2
73 = 97.3
74 = 97.4
75 = 97.5
76 = 97.6
77 = 97.7
78 = 97.8
79 = 97.9
80 = 98
81 = 98.1
82 = 98.2
83 = 98.3
84 = 98.4
85 = 98.5
86 = 98.6
87 = 98.7
88 = 98.8
89 = 98.9
90 = 99
91 = 99.1
92 = 99.2
93 = 99.3
94 = 99.4
95 = 99.5
96 = 99.6
97 = 99.7
98 = 99.8
99 = 99.9
100 = 100
