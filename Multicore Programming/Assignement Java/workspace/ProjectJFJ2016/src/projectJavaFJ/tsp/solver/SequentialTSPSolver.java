package projectJavaFJ.tsp.solver;
import projectJavaFJ.tsp.TravelingSalesmanProblem;
import projectJavaFJ.tsp.Tour;

/**
 * This class provides a sequential implementation of an exact TSP solver.
 * 
 * It performs an exhaustive depth first search on all semantically different tours.
 * A tour is presented by a permutation of city ids 0,...,n-1
 * 
 * In total there are n! syntactically different tours (permutations)
 * Of which (n-1)!/2 represent semantically different tours for the Euclidean TSP in general
 * (exploiting symmetry e.g. [0 1 2 3] = [3 2 1 0])
 * (exploiting shifts e.g. [0 1 2 3] = [3 0 1 2])
 * 
 * Consider the cities in a fixed order c0,...,cn-1
 * Let s_i be a tour for cities c0,...,ci-1
 * 
 * The algorithm recursively constructs all possible tours and returns the solution which minimises the distance travelled
 * 1- Start by recursively extending s0
 * 2- Given s_i, 
 *   i = n: s_i is a full tour
 *   i < n: 
 *   construct all tours s_(i+1) that retain the relative ordering of cities in s_i:
 *     i.e. visit c_(i+1) at any position in the route (first,...,last).
 *   recursively extend each s_(i+1)
 * 
 * @author Steven Adriaensen
 *
 */
public class SequentialTSPSolver implements TSPSolver{
	public static int visitNumber = 0;
	public Tour solve(TravelingSalesmanProblem problem){
		return find_optimal_tour(problem.getEmptySolution(),problem);
	}
	
	/**
	 * @param tour: the partial tour s_i to be extended
	 * @param problem: the tsp problem to be solved
	 * @return the best full tour for which the first i cities are visited in the order given by s_i
	 */
	public static Tour find_optimal_tour(Tour tour, TravelingSalesmanProblem problem){
		if(tour.getCitiesVisited() == problem.getNumberOfCities()){
			//don't extend a full tour further
			return tour;
		}else{
			//determine the id of the city we'll schedule next (c_(i+1))
			int cid = tour.getCitiesVisited();
			//pessimistic initialisation
			double min_distance_travelled = Double.POSITIVE_INFINITY;
			Tour best_sol = null;
			if(cid < 3){
				//only consider insertions at the back 
				//(exploit symmetry & shifts: consider only 1 tour for up to 3 cities)

				++visitNumber;
				return find_optimal_tour(tour.visit(cid,cid+1),problem);
			}else{
				//we'll consider scheduling city cid at each position in the tour
				//NOTE: we don't check the last position because in a tour first = last.
				for(int pos = 1; pos <= cid; pos++){
					//visit city cid as pos'th city in the current tour
					Tour new_tour = tour.visit(cid, pos);
					++visitNumber;
					//find the best extension of new tour (visiting unvisited cities)
					Tour temp_sol = find_optimal_tour(new_tour,problem);
					if(temp_sol.getDistanceTravelled() < min_distance_travelled){
						//we found a better tour
						min_distance_travelled = temp_sol.getDistanceTravelled();
						best_sol = temp_sol;
					}
				}
				return best_sol;
			}
		}
	}
	
	public String toString(){
		return "SequentialTSPSolver";
	}
	
}
