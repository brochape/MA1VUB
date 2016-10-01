package projectJavaFJ.benchmarks;


import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.io.UnsupportedEncodingException;

import projectJavaFJ.tsp.TravelingSalesmanProblem;
import projectJavaFJ.tsp.solver.ParallelTSPSolver;
import projectJavaFJ.tsp.solver.SequentialTSPSolver;
import projectJavaFJ.tsp.solver.TSPSolver;

public class Benchmark {
	private int amountOfRepetition;
	private long[][] times;

	public Benchmark(int amountOfRepetitions) {
		this.amountOfRepetition = amountOfRepetitions;
//		int n = 12;
//		Random val = new Random(0);
//		TravelingSalesmanProblem tsp = new TravelingSalesmanProblem(n, val);
//		TravelingSalesmanProblem tsp2 = new TravelingSalesmanProblem(n, val);
//		
//		//Initialise the solver
//		TSPSolver solverSeq = new SequentialTSPSolver();
//		TSPSolver solverPar = new ParallelTSPSolver(0);

		//solve tsp using solver
//		System.out.println("Solving "+tsp+" using "+solverSeq+"...");
//		long beforeSeq = System.currentTimeMillis();
//		Tour solSeq = solverSeq.solve(tsp);
//		long afterSeq = System.currentTimeMillis();
//		long timeSeq = afterSeq - beforeSeq;
//		System.out.println("Solving "+tsp2+" using "+solverPar+"...");
//		long beforePar = System.currentTimeMillis();
//		Tour solPar = solverPar.solve(tsp2);
//		long afterPar = System.currentTimeMillis();
//		long timePar = afterPar - beforePar;
//		System.out.println("Sequential : " + timeSeq);
//		System.out.println("Parallel : " + timePar + "ms");
		}
	private void save(String filename) {
		String res = "";
		for (int i = 0; i < times.length; i++) {
			for (int j = 0; j < times[i].length; j++) {
				res += times[i][j] + " - ";
			}
			res += "\n";
		}
//		System.out.println(res);
		PrintWriter out = null;
		try {
			out = new PrintWriter("./benchmarks/" + filename, "UTF-8");
		} catch (FileNotFoundException | UnsupportedEncodingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		out.println(res);
		out.close();
	}
	
	public void varyAmountOfTownsOneThread(int maxCities) {
		this.times = new long[maxCities+1][this.amountOfRepetition +10];
		for (int i = 0; i < this.amountOfRepetition + 10; i++) {// 10 useless runs to warm up the JVM
			for (int j = 0; j <= maxCities; j++) {
				TSPSolver solverPar = new ParallelTSPSolver(0,1);
				
				TravelingSalesmanProblem tsp = new TravelingSalesmanProblem(j);
				
				long beforePar = System.currentTimeMillis();
				solverPar.solve(tsp);
				long afterPar = System.currentTimeMillis();
				long timeTaken = afterPar - beforePar;
				this.times[j][i] = timeTaken;
				
				System.gc();
				
			}
		}
		save("amountOfCitiesOneThread.txt");
		
	}
	public void varyAmountOfTowns(int maxCities){
		this.times = new long[maxCities+1][this.amountOfRepetition +10];
		for (int i = 0; i < this.amountOfRepetition + 10; i++) {// 10 useless runs to warm up the JVM
			for (int j = 0; j <= maxCities; j++) {
				TSPSolver solverPar = new ParallelTSPSolver(0);
				
				TravelingSalesmanProblem tsp = new TravelingSalesmanProblem(j);
				
				long beforePar = System.currentTimeMillis();
				solverPar.solve(tsp);
				long afterPar = System.currentTimeMillis();
				long timeTaken = afterPar - beforePar;
				this.times[j][i] = timeTaken;
				
				System.gc();
				
			}
		}
		save("amountOfCitiesPar.txt");
	}
	
	public void varyAmountOfTownsSeq(int maxCities){
		this.times = new long[maxCities+1][this.amountOfRepetition +10];
		for (int i = 0; i < this.amountOfRepetition + 10; i++) {// 10 useless runs to warm up the JVM
			for (int j = 0; j <= maxCities; j++) {
				TSPSolver solverPar = new SequentialTSPSolver();
				
				TravelingSalesmanProblem tsp = new TravelingSalesmanProblem(j);
				
				long beforePar = System.currentTimeMillis();
				solverPar.solve(tsp);
				long afterPar = System.currentTimeMillis();
				long timeTaken = afterPar - beforePar;
				this.times[j][i] = timeTaken;
//				System.out.println(i + " - "+ j + ": " + timeTaken);
				
				System.gc();
				
			}
		}
		save("amountOfCitiesSeq.txt");
	}
	
	public void varyThresholdMaxThread(int threshold){
		this.times = new long[threshold][this.amountOfRepetition +10];
		for (int i = 0; i < this.amountOfRepetition + 10; i++) {// 10 useless runs to warm up the JVM
			for (int j = 0; j < threshold; j++) {
				TSPSolver solverPar = new ParallelTSPSolver(j);
				
				TravelingSalesmanProblem tsp = new TravelingSalesmanProblem(12);
				
				long beforePar = System.currentTimeMillis();
				solverPar.solve(tsp);
				long afterPar = System.currentTimeMillis();
				long timeTaken = afterPar - beforePar;
				this.times[j][i] = timeTaken;
				
				System.gc();
				
			}
		}
		save("thresholdPar.txt");
		
	}	
	public void varyThresholdOneThread(int threshold){
		this.times = new long[threshold][this.amountOfRepetition +10];
		for (int i = 0; i < this.amountOfRepetition + 10; i++) {// 10 useless runs to warm up the JVM
			for (int j = 0; j < threshold; j++) {
				TSPSolver solverPar = new ParallelTSPSolver(j,1);
				
				TravelingSalesmanProblem tsp = new TravelingSalesmanProblem(12);
				
				long beforePar = System.currentTimeMillis();
				solverPar.solve(tsp);
				long afterPar = System.currentTimeMillis();
				long timeTaken = afterPar - beforePar;
				this.times[j][i] = timeTaken;
				
				System.gc();
				
			}
		}
		save("thresholdOneThread.txt");
		
	}	
	
	public void varyThresholdSeq(){
		this.times = new long[1][this.amountOfRepetition + 10];
		for (int i = 0; i < this.amountOfRepetition + 10; i++) {// 10 useless runs to warm up the JVM
			TSPSolver solverPar = new SequentialTSPSolver();
			TravelingSalesmanProblem tsp = new TravelingSalesmanProblem(12);
			
			long beforePar = System.currentTimeMillis();
			solverPar.solve(tsp);
			long afterPar = System.currentTimeMillis();
			long timeTaken = afterPar - beforePar;
			this.times[0][i] = timeTaken;
			
			System.gc();
		}
		save("thresholdSeq.txt");
		
	}
	
	
	public void varyAmountOfThreads(){
		int maxThreads = Runtime.getRuntime().availableProcessors();
		this.times = new long[maxThreads][this.amountOfRepetition +10];
		for (int i = 0; i < this.amountOfRepetition + 10; i++) {// 10 useless runs to warm up the JVM
			for (int j = 0; j < maxThreads; j++) {
				TSPSolver solverPar = new ParallelTSPSolver(5, j+1);
				
				TravelingSalesmanProblem tsp = new TravelingSalesmanProblem(12);
				
				long beforePar = System.currentTimeMillis();
				solverPar.solve(tsp);
				long afterPar = System.currentTimeMillis();
				long timeTaken = afterPar - beforePar;
				this.times[j][i] = timeTaken;
				
				System.gc();
				
			}
		}
		save("amountOfThreadsPar.txt");
		
		
	}

	
}
