import numpy as np
import matplotlib.pyplot as plt
import math

def main():
    # threads()
    # cities()
    threshold()
    # noThreshold()

def noThreshold():
    # pass
    # threads()
    # avrgInsertionPerTask()
    # totalTask()
    # totalInsertion()
    # totalInsCrit()
    execTime()
    # workOverSpan()

def threshold():
    # overheadThreshold()
    # tasksT()
    diff()
    # tworkOverSpan()

def overheadThreshold():
    seq = []
    with open("thresholdSeq.txt", 'r') as f:
        for line in f.readlines():
            line = line.strip()[:-2].split(" - ")[10:]
            seq.append(sum([int(a) for a in line])/len(line))
    oneT = []
    with open("thresholdOneThread.txt", 'r') as f:
        for line in f.readlines():
            line = line.strip()[:-2].split(" - ")[10:]
            oneT.append(sum([int(a) for a in line])/len(line))

    maxT = []
    with open("thresholdPar.txt", 'r') as f:
        for line in f.readlines():
            line = line.strip()[:-2].split(" - ")[10:]
            maxT.append(sum([int(a) for a in line])/len(line))

    # overhead = [oneT[i]/seq[0] for i in range(len(oneT))]
    # plt.plot(overhead)
    # plt.title('Overhead of the Parallel implementation for different threshold values')
    # plt.xlabel('Threshold value')
    # plt.ylabel('Overhead')
    # plt.show()


    computationalSpeedups = []
    for i in range(len(maxT)):
        computationalSpeedups.append(oneT[i]/maxT[i])
        print("A " + str(oneT[i]))
        print("B " + str(maxT[i]))

    applicationSpeedups = []
    for i in range(len(maxT)):
        applicationSpeedups.append(seq[0]/maxT[i])


    comp, = plt.plot([i for i in range(len(maxT))],computationalSpeedups,color='r')
    app, = plt.plot([i for i in range(len(maxT))],applicationSpeedups,color='b')


    plt.legend([comp, app], [r'Computational speedup ($\frac{T_1}{T_P}$)', 'Application speedup ('+r' $\frac{T_{seq}}{T_{P}}$)'])
    plt.title('Influence of the value of the threshold on the speedups')
    plt.xlabel('Value of the threshold')
    plt.ylabel('Ratio')
    plt.show()


def tworkOverSpan():
    n = 12
    work = [(i)/1.0 if i<4 else (3+1/2.0*sum([math.factorial(k) for k in range(3,n-1)])) for i in range(1,15)]
    span = [(i)/1.0 if i<4 else (6 + 1/2.0*sum([math.factorial(k) for k in range(3,i-1)]) + 1/2.0*sum([k for k in range(3,n-i-1)])) for i in range(1,15)]
    para = [work[i]/span[i] for i in range(len(span))]
    plt.plot(para)
    plt.title("Average parallelism for different thresholds")
    plt.xlabel('Threshold')
    plt.ylabel('Value of the parallelism')
    plt.show()

def totalInsertion():
    sumArray = [(i+1)/1.0 if i<3 else 3+1/2.0*sum([math.factorial(k) for k in range(3,i+1)]) for i in range(15)]
    plt.title("Amount of insertions performed for different amounts of cities")
    plt.xlabel('Amount of cities')
    plt.ylabel('#insertions')
    plt.plot(sumArray)
    plt.show()

def totalTask():
    sumArray = [1.0 if i<4 else 1/2.0*math.factorial(i-1) for i in range(1,15)]
    plt.title("Amount of tasks created for different amounts of cities")
    plt.xlabel('Amount of cities')
    plt.ylabel('#tasks')
    plt.plot([i+1 for i in range(len(sumArray))],sumArray)
    plt.show()

def avrgInsertionPerTask():
    insertionArray = [i/1.0 if i<4 else 3+1/2.0*sum([math.factorial(k) for k in range(3,i)]) for i in range(1,15)]
    taskArray = [1.0 if i<4 else 1/2.0*math.factorial(i-1) for i in range(1,15)]
    diffArray = [insertionArray[i]/taskArray[i] for i in range(len(insertionArray))]
    plt.title("Average amount of insertion per tasks for different amounts of cities")
    plt.xlabel('Amount of cities')
    plt.ylabel('#insertion per task')
    plt.plot([i+1 for i in range(len(diffArray))],diffArray)
    plt.show()

def totalInsCrit():
    sumArray = [i/1.0 if i<4 else 3+1/2.0*sum([k for k in range(3,i)]) for i in range(1,15)]
    plt.title("Total amount of insertions on the critical path for different amounts of cities")
    plt.xlabel('Amount of cities')
    plt.ylabel('#tasks on the critical path')
    plt.plot([i+1 for i in range(len(sumArray))],sumArray)
    plt.show()

def workOverSpan():
    workArray = [i/1.0 if i<4 else 3+1/2.0*sum([math.factorial(k) for k in range(3,i)]) for i in range(1,15)]
    spanArray = [i/1.0 if i<4 else 3+1/2.0*sum([k for k in range(3,i)]) for i in range(1,15)]
    diffArray = [workArray[i]/spanArray[i] for i in range(len(workArray))]
    plt.title("Average parallelism for different amounts of cities")
    plt.xlabel('Amount of cities')
    plt.ylabel('Parallelism')
    plt.plot([i+1 for i in range(len(diffArray))],diffArray)
    plt.show()


def execTime():
    seq = []
    with open("amountOfCitiesSeq.txt", 'r') as f:
        for line in f.readlines():
            line = line.strip()[:-2].split(" - ")[10:]
            seq.append([int(a) for a in line])
            print(seq)
    plt.boxplot(seq, notch=0, sym='+', vert=1, whis=1.5)
    plt.title("Execution time of sequential implementation")
    plt.xlabel('Amount of cities')
    plt.ylabel('Execution time (ms)')
    plt.show()

def tasksT():
    n = 12
    tasksTh = [] 
    for i in range(1,15):
        if n-i <3:
            tasksTh.append(n)
        elif i == 0:
            tasksTh.append(math.factorial(n-1)/2.0)
        else:
            tasksTh.append(math.factorial(n-i))
    # plt.plot(tasksTh)
    # plt.title('Amount of tasks created as a function of the value of the threshold')
    # plt.xlabel('Value of the threshold')
    # plt.ylabel('#tasks')
    # plt.show()
    diffArray = [sumArray[11]/tasksTh[i] for i in range(len(tasksTh))]
    plt.plot(diffArray)
    plt.title('Amount of insertions per task created as a function of the value of the threshold')
    plt.xlabel('Value of the threshold')
    plt.ylabel('#insertions per task')
    plt.show()

def diff():
    n = 12
    diffArray = [] 
    for i in range(1,15):
        if n-i <3:
            if n<3:
                diffArray.append(((i+1)/1.0)/n)
            else:
                diffArray.append((3+1/2.0*sum([math.factorial(k) for k in range(3,i+1)]))/n)

        elif i == 0:
            if n<3:
                diffArray.append(((i+1)/1.0)/(math.factorial(n-1)/2.0))
            else:
                diffArray.append((3+1/2.0*sum([math.factorial(k) for k in range(3,i+1)]))/(math.factorial(n-1)/2.0))
        else:
            if n<3:
                diffArray.append(((i+1)/1.0)/math.factorial(n-i))
            else:
                diffArray.append((3+1/2.0*sum([math.factorial(k) for k in range(3,i+1)]))/math.factorial(n-i))

    plt.plot(diffArray)
    plt.title('Amount of insertions per task created as a function of the value of the threshold')
    plt.xlabel('Value of the threshold')
    plt.ylabel('#insertions per task')
    plt.show()





def threads():
    threads = []
    with open("amountOfThreadsPar.txt", 'r') as f:
        for line in f.readlines():
            line = line.strip()[:-2].split(" - ")[10:]
            threads.append(sum([int(a) for a in line])/len(line))
    computationalSpeedups = []
    for i in range(len(threads)):
        computationalSpeedups.append(threads[0]/threads[i])

    seq = cities()

    applicationSpeedups = []
    for i in range(len(threads)):
        applicationSpeedups.append(seq/threads[i])

    efficiencies = []
    for i in range(len(threads)):
        efficiencies.append(threads[0]/((i+1)*threads[i]))

    comp, = plt.plot([i+1 for i in range(len(threads))],computationalSpeedups,color='r')
    app, = plt.plot([i+1 for i in range(len(threads))],applicationSpeedups,color='b')
    eff, = plt.plot([i+1 for i in range(len(threads))],efficiencies,color='g')


    plt.legend([comp, app, eff], [r'Computational speedup ($\frac{T_{1}}{T_{P}}$)', r'Application speedup ($\frac{T_{seq}}{T_{P}}$)', r'Efficiency ($\frac{T_{1}}{PT_{P}}$)'])
    plt.title('Influence of the amount of threads on the speedups')
    plt.xlabel('Amount of threads used')
    plt.ylabel('Ratio')
    plt.show()

    # fig, ax1 = plt.subplots()
    # plt.subplots_adjust(left=0.075, right=0.95, top=0.9, bottom=0.25)
    # print("COMPUTATIONAL: " + str(speedup))
    # speedup = seq/threads[-1]
    # print("APPLICATION: " + str(speedup))


def cities():
    oneThread = []
    with open("amountOfCitiesOneThread.txt", 'r') as f:
        for line in f.readlines():
            line = line.strip()[:-2].split(" - ")[10:]
            oneThread.append(sum([int(a) for a in line])/len(line))
    eightThread = []
    with open("amountOfCitiesPar.txt", 'r') as f:
        for line in f.readlines():
            line = line.strip()[:-2].split(" - ")[10:]
            eightThread.append(sum([int(a) for a in line])/len(line))
    seq = []
    with open("amountOfCitiesSeq.txt", 'r') as f:
        for line in f.readlines():
            line = line.strip()[:-2].split(" - ")[10:]
            seq.append(sum([int(a) for a in line])/len(line))
    return seq[12]

    overhead = []
    for i in range(len(oneThread)):
        if seq[i] != 0:
            overhead.append(oneThread[i]/seq[i])
        else:
            overhead.append(0)
        
    plt.plot(overhead)
    plt.title('Influence of the amount of cities on the overhead')
    plt.xlabel('Amount of cities')
    plt.ylabel('T_1/T_seq')
    plt.show()





if __name__ == '__main__':
    main()
