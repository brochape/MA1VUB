for i in {1..4}
do
    #echo "---"
    #echo "> fib, $i threads"
    #erl +S $i -noshell -s dec_benchmark test_fib -s init stop > results/64/decentralized/output-fib-$i.txt
    # echo "---"
    # echo "> get_channel_history, $i threads"
    # erl +S $i -noshell -s dec_benchmark test_get_channel_history -s init stop > results/4/decentralized/output-get_channel_history-$i.txt
    # echo "---"
    echo "> send_message long, $i threads"
    erl +S $i -noshell -s dec_benchmark test_send_message_long -s init stop > results/4/decentralized/output-send_message-$i.txt

    echo "---"
    echo "> send_message short, $i threads"
    erl +S $i -noshell -s dec_benchmark test_send_message_short -s init stop > results/4/decentralized/output-send_message-short-$i.txt

    echo "---"
    echo "> send_message mini, $i threads"
    erl +S $i -noshell -s dec_benchmark test_send_message_mini -s init stop > results/4/decentralized/output-send_message-mini-$i.txt

    echo "---"
    # echo "> try_latency, $i threads"
    # erl +S $i -noshell -s dec_benchmark test_measure_latency -s init stop > results/4/decentralized/output-latency-$i.txt


done
