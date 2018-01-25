<?php
    set_time_limit(0);
    ini_set('memory_limit', '2048M');
    $data = addslashes (file_get_contents('php://input'));
    $start = round(microtime(true) * 1000);
    $res = shell_exec("../Firewall \" ". $data ." \"");
    $stop = round(microtime(true) * 1000);

    $bench = "";
    if (file_exists("../Bench/Bench_Analyse_Plan_Redundancy.txt"))
        $bench = file_get_contents("../Bench/Bench_Analyse_Plan_Redundancy.txt");

    if($bench == ""){
        $bench = array();
    }else {
        $bench = json_decode($bench);
    }
    $bench[]=array(
            "start" => $start,
            "stop" => $stop,
            "time" => $stop - $start);

    file_put_contents("../Bench/Bench_Analyse_Plan_Redundancy.json", json_encode($bench), FILE_APPEND);
    echo trim(trim(stripcslashes($res)), "\"");
?>
