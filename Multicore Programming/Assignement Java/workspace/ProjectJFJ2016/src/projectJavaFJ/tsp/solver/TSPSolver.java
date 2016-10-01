package projectJavaFJ.tsp.solver;

import projectJavaFJ.tsp.Tour;
import projectJavaFJ.tsp.TravelingSalesmanProblem;

/**
 * An interface for exact TSP solvers
 * 
 * @author Steven Adriaensen
 *
 */
public interface TSPSolver {
	Tour solve(TravelingSalesmanProblem vrp);
}
