package projectJavaFJ.tsp.solver;

import projectJavaFJ.tsp.TravelingSalesmanProblem;

import java.util.Arrays;
import java.util.Iterator;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.RecursiveTask;

import org.hamcrest.core.IsNull;

import projectJavaFJ.tsp.Tour;

/**
 * A parallel version of SequentialTSPSolver using Java Fork Join
 * 
 * @author You
 *
 */
public class ParallelTSPSolver implements TSPSolver{
	final private int p; //the number of working threads to be used
	final private long T; //the value of the sequential threshold
	public int insertionsAmount ;
	
	/**
	 * Constructs an instance of ParallelTSPSolver,
	 * using a sequential threshold value of T >= 0
	 * and a number of working threads equal to the # logical processors.
	 * @param T the value of the sequential threshold
	 */
	public ParallelTSPSolver(long T){
		this(T,Runtime.getRuntime().availableProcessors());
		
	}
	
	/**
	 * Constructs an instance of ParallelTSPSolver,
	 * using a sequential threshold value of T >= 0
	 * and p working threads.
	 * 
	 * @param T the value of the sequential threshold
	 * @param p the number of working threads
	 */
	public ParallelTSPSolver(long T, int p){
		this.T = T;
		this.p = p;
		
	}
	protected Tour shortestTour(Tour[] tourList){
		Tour res = tourList[0];
		for (Tour tour : tourList){
			if (tour.getDistanceTravelled() < res.getDistanceTravelled()){
				res = tour;
			}
		}
		return res;
	}
	
	public Tour solve(TravelingSalesmanProblem problem){
		insertionsAmount = 1
				;
		if (problem.getNumberOfCities() == 0) {
			return problem.getEmptySolution();
		} else {
			Tour[] tours = {problem.getEmptySolution().visit(0, 1)};
			ForkJoinPool fjp = new ForkJoinPool(p);
			return fjp.invoke(new TSPSolverTask(problem, tours));
		}
	}
	private class TSPSolverTask extends RecursiveTask<Tour> {

		public TravelingSalesmanProblem problem;
		private Tour[] allTours;
		
		public TSPSolverTask(TravelingSalesmanProblem problem, Tour[] allTours) {
			this.problem = problem;
			this.allTours = allTours;
			
		}
		@Override
		protected Tour compute() {
			int cid = allTours[0].getCitiesVisited();//Amount of cities visited, they all have the same amount on each step
			int totalCities = problem.getNumberOfCities();
			if (allTours.length != 1) {
				//Split task into 2 subtask
				int half = allTours.length / 2;
				Tour[] firstHalf = Arrays.copyOfRange(allTours, 0, half);//Split the array of tours into 2 recursively until it's only of size one
				allTours =  Arrays.copyOfRange(allTours, half, allTours.length);
				
				TSPSolverTask left = new TSPSolverTask(problem, firstHalf);
				
				left.fork();
				Tour shortestRight = compute();//simple recursive task without spawning a new task
				Tour shortestLeft = left.join();
				Tour[] shortestPaths = {shortestLeft, shortestRight};
				return shortestTour(shortestPaths);//Compare the left subtree solution to the right one and return the shortest one
			}
			else{
				if (cid == totalCities) {//leaf
					return allTours[0];
					
				}
				else{
					int taskSize = totalCities - cid; //Compute amount of work done by task Double.POSITIVE_INFINITY; 
					if (taskSize < T){
						allTours[0] = SequentialTSPSolver.find_optimal_tour(allTours[0],problem);
						return allTours[0];
					}
					else{
						if(cid < 3){
							//only consider insertions at the back 
							//(exploit symmetry & shifts: consider only 1 tour for up to 3 cities)
							allTours[0] = allTours[0].visit(cid,cid+1);//visit next city
							++insertionsAmount;
						}
						else{
							if (allTours.length == 1){
								int nextToursSize = cid;
								Tour[] nextTours = new Tour[nextToursSize];
								//Visit the next city in different positions
								for (int j = 0; j < cid; ++j) {
									nextTours[j] = allTours[0].visit(cid, j+1);
									++insertionsAmount;
								}
								allTours = nextTours;
							}
						}
						return compute();
					}
				}
			}
		}
		
	}
	
	public void safePrintln(Object s) {
		  synchronized (System.out) {
		    System.out.println(s);
		  }
		}
	public void safePrint(String s) {
		  synchronized (System.out) {
		    System.out.print(s);
		  }
		}
	
	public String toString(){
		return "ParallelTSPSolver(p: "+p+", T: "+T+")";
	}
	
}
