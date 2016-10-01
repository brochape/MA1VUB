package projectJavaFJ;

import static org.junit.Assert.*;

import java.util.Random;

import org.junit.Test;

import projectJavaFJ.benchmarks.Benchmark;
import projectJavaFJ.tsp.Tour;
import projectJavaFJ.tsp.TravelingSalesmanProblem;
import projectJavaFJ.tsp.solver.ParallelTSPSolver;
import projectJavaFJ.tsp.solver.SequentialTSPSolver;
import projectJavaFJ.tsp.solver.TSPSolver;
import projectJavaFJ.tsp.test.TravelingSalesmanProblemTest;
import projectJavaFJ.tsp.test.TourTest;

/**
 * 
 * Illustrates how tsp instances can be generated and solved using a tsp solver. <br>
 * It also contains unit tests that test the correctness of ParallelTSPSolver.
 * 
 * @author Steven Adriaensen
 *
 */
public class Main {

	/**
	 * 
	 * Code fragment illustrating how tsp instances can be generated and solved using a tsp solver.
	 * Feel free to change this code!
	 * 
	 * @param args command-line arguments (none)
	 */
	public static void main(String[] args) {
		
		Benchmark bench = new Benchmark(30);
//		bench.varyAmountOfTownsOneThread(13);
//		bench.varyAmountOfTowns(13);
//		bench.varyAmountOfTownsSeq(13);
//		int[] tresholds = {0,5,10};
//		bench.varyThresholdSeq();
		bench.varyThresholdOneThread(14);
//		bench.varyThresholdMaxThread(14);
//		bench.varyAmountOfThreads();
//		int maxCities = 12;
//		int[] times = new int[maxCities+1];
//		for (int j = 1; j <= maxCities; j++) {
//			ParallelTSPSolver solverPar = new ParallelTSPSolver(0);
//			
//			TravelingSalesmanProblem tsp = new TravelingSalesmanProblem(j);
//			
//			solverPar.solve(tsp);
//			System.out.println(j + " cities : "+ solverPar.insertionsAmount + " insertions");
//			System.gc();
//			
//		}
	}
	
	/**
	 * Unit test testing ParallelTSPSolver using threshold 0. <br>
	 */
	@Test
	public void test1(){
		int seed = 0; //you can change this value for additional tests.
		//please leave the code beyond this point untouched (you can add additional tests as a separate test)
		int T = 0;
		TravelingSalesmanProblemTest tsp;
		TSPSolver solver_seq = new SequentialTSPSolver();
		TSPSolver solver_par = new ParallelTSPSolver(T);
		TourTest sol_seq;
		TourTest sol_par;
		
		//instance with 0 cities
		tsp = new TravelingSalesmanProblemTest(0,new Random(seed));
		sol_seq = (TourTest) solver_seq.solve(tsp);
		sol_par = (TourTest) solver_par.solve(tsp);
		assertEquals(sol_seq.getDistanceTravelled(),sol_par.getDistanceTravelled(),0.01); // correctness: same optimal distance
		assertEquals(sol_seq.toString(),sol_par.toString()); //identity test: same optimal solution
		assertEquals(sol_seq.getNumberOfInsertions(),sol_par.getNumberOfInsertions()); //identity test: same # insertions
		
		//instance with 1 city
		tsp = new TravelingSalesmanProblemTest(1,new Random(seed+11));
		sol_seq = (TourTest) solver_seq.solve(tsp);
		sol_par = (TourTest) solver_par.solve(tsp);
		assertEquals(sol_seq.getDistanceTravelled(),sol_par.getDistanceTravelled(),0.01); // correctness: same optimal distance
		assertEquals(sol_seq.toString(),sol_par.toString()); //identity test: same optimal solution
		assertEquals(sol_seq.getNumberOfInsertions(),sol_par.getNumberOfInsertions()); //identity test: same # insertions
		
		//instance with 10 cities
		tsp = new TravelingSalesmanProblemTest(10,new Random(seed+7));
		sol_seq = (TourTest) solver_seq.solve(tsp);
		sol_par = (TourTest) solver_par.solve(tsp);
		assertEquals(sol_seq.getDistanceTravelled(),sol_par.getDistanceTravelled(),0.01); // correctness: same optimal distance
		assertEquals(sol_seq.toString(),sol_par.toString()); //identity test: same optimal solution
		assertEquals(sol_seq.getNumberOfInsertions(),sol_par.getNumberOfInsertions()); //identity test: same # insertions
	}
	
	/**
	 * Unit test testing ParallelTSPSolver using an intermediary threshold value. <br>
	 * You can change this value to be more appropriate for the threshold value you've implemented.
	 */
	@Test
	public void test2(){
		int seed = 0; //you can change this value for additional tests.
		long T = 5; //you can change this value to be more appropriate for the threshold you've implemented
		//please leave the code beyond this point untouched (you can add additional tests as a separate test)
		TravelingSalesmanProblemTest vrp;
		TSPSolver solver_seq = new SequentialTSPSolver();
		TSPSolver solver_par = new ParallelTSPSolver(T);
		TourTest sol_seq;
		TourTest sol_par;
		
		//instance with 1 city
		vrp = new TravelingSalesmanProblemTest(1,new Random(seed));
		sol_seq = (TourTest) solver_seq.solve(vrp);
		sol_par = (TourTest) solver_par.solve(vrp);
		assertEquals(sol_seq.getDistanceTravelled(),sol_par.getDistanceTravelled(),0.01); // correctness: same optimal distance
		assertEquals(sol_seq.toString(),sol_par.toString()); //identity test: same optimal solution
		assertEquals(sol_seq.getNumberOfInsertions(),sol_par.getNumberOfInsertions()); //identity test: same # insertions
		
		//instance with 5 cities
		vrp = new TravelingSalesmanProblemTest(5,new Random(seed+11));
		sol_seq = (TourTest) solver_seq.solve(vrp);
		sol_par = (TourTest) solver_par.solve(vrp);
		assertEquals(sol_seq.getDistanceTravelled(),sol_par.getDistanceTravelled(),0.01); // correctness: same optimal distance
		assertEquals(sol_seq.toString(),sol_par.toString()); //identity test: same optimal solution
		assertEquals(sol_seq.getNumberOfInsertions(),sol_par.getNumberOfInsertions()); //identity test: same # insertions
		
		//instance with 10 cities
		vrp = new TravelingSalesmanProblemTest(10,new Random(seed+7));
		sol_seq = (TourTest) solver_seq.solve(vrp);
		sol_par = (TourTest) solver_par.solve(vrp);
		assertEquals(sol_seq.getDistanceTravelled(),sol_par.getDistanceTravelled(),0.01); // correctness: same optimal distance
		assertEquals(sol_seq.toString(),sol_par.toString()); //identity test: same optimal solution
		assertEquals(sol_seq.getNumberOfInsertions(),sol_par.getNumberOfInsertions()); //identity test: same # insertions
	}
	
	
	/**
	 * Unit test testing ParallelTSPSolver using an extremely high threshold value. <br>
	 * Note that ParallelTSPSolver should only create a single task in this case (average parallelism = 1).
	 */
	@Test
	public void test3(){
		int seed = 0; //you can change this value for additional tests.
		//please leave the code beyond this point untouched (you can add additional tests as a separate test)
		long T = Long.MAX_VALUE;
		TravelingSalesmanProblemTest vrp;
		TSPSolver solver_seq = new SequentialTSPSolver();
		TSPSolver solver_par = new ParallelTSPSolver(T);
		TourTest sol_seq;
		TourTest sol_par;
		
		//instance with 1 city
		vrp = new TravelingSalesmanProblemTest(10,new Random(seed));
		sol_seq = (TourTest) solver_seq.solve(vrp);
		sol_par = (TourTest) solver_par.solve(vrp);
		assertEquals(sol_seq.getDistanceTravelled(),sol_par.getDistanceTravelled(),0.01); // correctness: same optimal distance
		assertEquals(sol_seq.toString(),sol_par.toString()); //identity test: same optimal solution
		assertEquals(sol_seq.getNumberOfInsertions(),sol_par.getNumberOfInsertions()); //identity test: same # insertions
		
		//instance with 5 cities
		vrp = new TravelingSalesmanProblemTest(5,new Random(seed+11));
		sol_seq = (TourTest) solver_seq.solve(vrp);
		sol_par = (TourTest) solver_par.solve(vrp);
		assertEquals(sol_seq.getDistanceTravelled(),sol_par.getDistanceTravelled(),0.01); // correctness: same optimal distance
		assertEquals(sol_seq.toString(),sol_par.toString()); //identity test: same optimal solution
		assertEquals(sol_seq.getNumberOfInsertions(),sol_par.getNumberOfInsertions()); //identity test: same # insertions
		
		//instance with 10 cities
		vrp = new TravelingSalesmanProblemTest(10,new Random(seed+7));
		sol_seq = (TourTest) solver_seq.solve(vrp);
		sol_par = (TourTest) solver_par.solve(vrp);
		assertEquals(sol_seq.getDistanceTravelled(),sol_par.getDistanceTravelled(),0.01); // correctness: same optimal distance
		assertEquals(sol_seq.toString(),sol_par.toString()); //identity test: same optimal solution
		assertEquals(sol_seq.getNumberOfInsertions(),sol_par.getNumberOfInsertions()); //identity test: same # insertions
	}
}