<?php

    $m = intval (date("i") /5);

    $load = 0;
    switch ($m) {
        case 0: // [0,5] minutes
            $load = 10; // percent
        case 1: // ]5,10] minutes
            $load = 40; // percent
        case 2: // ]10,15] minutes
            $load = 75; // percent
        case 3: // ]15,20] minutes
            $load = 58; // percent
        case 4: // ]20,25] minutes
            $load = 20; // percent
        case 5: // ]25,30] minutes
            $load = 85; // percent
        case 6: // ]30,35] minutes
            $load = 10; // percent
        case 7: // ]35,40] minutes
            $load = 63; // percent
        case 8: // ]40,45] minutes
            $load = 32; // percent
        case 9; // ]45,50] minutes
            $load = 92; // percent
        case 10: // ]50,55] minutes
            $load = 68; // percent
        case 11; // ]55,60] minutes
            $load = 40; // percent
    }

    shell_exec("stress-ng -c 0 -l ". $load. " -t 360");
?>
