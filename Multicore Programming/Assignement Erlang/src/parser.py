import numpy as np
import matplotlib.pyplot as plt

CPUMEAN = 0
WALLCLOCKMEAN = 1

cputimes = []
wallclocktimes = []
cpumeans = []
wallclockmeans = []
testNumber = 30
everything = {}
tests={}
percoremax = {}
availabletests = {"decentralized":["get_channel_history", "latency", "send_message", "send_message-short", "send_message-mini"], 
                  "centralized":["get_channel_history", "send_message"]}

for coremax in [4,64]:
    for centralized in ["centralized", "decentralized"]:
        for test in availabletests[centralized]:
            for core in range(1, coremax+1):
                filename = "results/"+str(coremax) + "/" + centralized + "/output-" + test + "-" + str(core) + ".txt"
                with open(filename) as f:
                    for i in range(testNumber):
                        f.readline()
                        line = f.readline().strip().split(" = ")
                        cputimes.append(float(line[-1][:-3]))
                        wallclocktimes.append(float(f.readline().strip().split(" = ")[-1][:-3]))
                        f.readline()
                cpumeans.append(np.mean(cputimes))
                wallclockmeans.append(np.mean(wallclocktimes))
                cputimes = []
                wallclocktimes = []
            tests[test] = [cpumeans, wallclockmeans]
            cpumeans = []
            wallclockmeans = []
        percoremax[centralized] = tests
        tests = {}
    everything[coremax] = percoremax
    percoremax = {}

# Example of usage
decsend64 = everything[64]["decentralized"]["send_message"][CPUMEAN]
centsend64 = everything[64]["centralized"]["send_message"][CPUMEAN]

dechist64 = everything[64]["decentralized"]["get_channel_history"][CPUMEAN]
centhist64 = everything[64]["centralized"]["get_channel_history"][CPUMEAN]

declat64 = everything[64]["decentralized"]["latency"][CPUMEAN]


def send_message():
    f1 = plt.figure()
    line_up, = plt.plot([i+1 for i in range(64)],decsend64,'r-')
    line_down, = plt.plot([i+1 for i in range(64)],centsend64,'g-')
    plt.legend([line_up, line_down], ['Decentralized server', 'Centralized server'],loc = 5)
    # plt.axis([1,64,0,500])
    font = {'size'   : 12}
    plt.title("Means of the time taken by the CPU per number of cores\n send_message benchmark - 64 cores",font)
    plt.grid(True)
    plt.xlabel('Number of cores used')
    plt.ylabel('Time taken by the CPU (ms)')
    plt.savefig("graphs/meanCPU-SendMessage-64.png")

def get_history():
    f2 = plt.figure()
    line_up, = plt.plot([i+1 for i in range(64)],dechist64,'r-')
    line_down, = plt.plot([i+1 for i in range(64)],centhist64,'g-')
    plt.legend([line_up, line_down], ['Centralized server', 'Decentralized server'],loc = 5)
    # plt.axis([1,64,0,500])
    font = {'size'   : 12}
    plt.title("Means of the time taken by the CPU per number of cores\n get_channel_history benchmark",font)
    plt.grid(True)
    plt.xlabel('Number of cores used')
    plt.ylabel('Time taken by the CPU (ms)')
    plt.savefig("graphs/meanCPU-GetHistory.png")

def latency():
    f3 = plt.figure()
    line_up, = plt.plot([i+1 for i in range(64)],declat64,'r-')
    plt.legend([line_up], ['Decentralized server'],loc = 5)
    font = {'size'   : 12}
    plt.title("Means of the time taken by the CPU per number of cores\n latency benchmark",font)
    plt.grid(True)
    plt.xlabel('Number of cores used')
    plt.ylabel('Time taken by the CPU (ms)')
    plt.savefig("graphs/meanCPU-latency.png")

def send_message4():
    decsend4 = everything[4]["decentralized"]["send_message"][CPUMEAN]
    centsend4 = everything[4]["centralized"]["send_message"][CPUMEAN]
    f1 = plt.figure()
    line_up, = plt.plot([i+1 for i in range(4)],decsend4,'r-')
    line_down, = plt.plot([i+1 for i in range(4)],centsend4,'g-')
    plt.legend([line_up, line_down], ['Decentralized server', 'Centralized server'],loc = 5)
    # plt.axis([1,64,0,500])
    font = {'size'   : 12}
    plt.title("Means of the time taken by the CPU per number of cores\n send_message benchmark - 4 cores",font)
    plt.grid(True)
    plt.xlabel('Number of cores used')
    plt.ylabel('Time taken by the CPU (ms)')
    plt.savefig("graphs/meanCPU-SendMessage-4.png")

def send_message_small(corenumber):

    decsend = everything[corenumber]["decentralized"]["send_message-short"][CPUMEAN]
    f1 = plt.figure()
    line_up, = plt.plot([i+1 for i in range(corenumber)],decsend,'r-')
    plt.legend([line_up], [ 'Decentralized server'],loc = 5)
    # plt.axis([1,64,0,500])
    font = {'size'   : 12}
    plt.title("Means of the time taken by the CPU per number of cores\n send_message_small benchmark - "+str(corenumber)+" cores",font)
    plt.grid(True)
    plt.xlabel('Number of cores used')
    plt.ylabel('Time taken by the CPU (ms)')
    plt.savefig("graphs/meanCPU-SendMessageSmall"+str(corenumber)+".png")





def main():
    send_message()
    get_history()
    latency()
    send_message4()
    send_message_small(4)
    send_message_small(64)

if __name__ == '__main__':
    main()
