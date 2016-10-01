package projectJavaFJ.tsp;

/**
 * This class represents a (partial) solution to the VehicleRoutingProblem
 * @author Steven Adriaensen
 *
 */
public class Tour {
	final private TravelingSalesmanProblem tsp;
	final private int[] tour;
	final private double distance_travelled;
	
	/**
	 * Constructs a specific tour
	 * DO NOT USE THIS METHOD IN YOUR CODE
	 * 
	 * @param tsp
	 * @param tour
	 * @param distance_travelled
	 */
	private Tour(TravelingSalesmanProblem tsp, int[] tour, double distance_travelled){
		this.tsp = tsp;
		this.tour = tour;
		this.distance_travelled = distance_travelled;
	}
	
	/**
	 * Constructs a (shallow) copy of sol
	 * DO NOT USE THIS METHOD IN YOUR CODE
	 */
	protected Tour(Tour sol){
		this.distance_travelled = sol.distance_travelled;
		this.tour = sol.tour;
		this.tsp = sol.tsp;
	};
	
	/**
	 * Constructs an empty solution;
	 * DO NOT USE THIS METHOD IN YOUR CODE (call getEmptySolution() on the VehicleRoutingProblem to obtain a new solution)
	 */
	protected Tour(TravelingSalesmanProblem tsp){
		this.tsp = tsp;
		tour = new int[0];
		distance_travelled = 0;
	}
	
	/**
	 * Visit a given city as the pos'th city in the route.
	 * 
	 * @param city ID of the city to be visited, an integer in the range [0,n-1]
	 * @param pos Indicates the position at which the city must be visited into the route (integer in range [1,getCitiesVisited()+1])
	 * @return A solution identical to this one, with the city visited as the pos'th city in the tour.
	 */
	public Tour visit(int city, int pos){
		//derive insertion index
		int index = pos-1;
		
		//create the new tour
		int[] new_tour = new int[tour.length+1];
		System.arraycopy(tour, 0, new_tour, 0, index);
		System.arraycopy(tour, index, new_tour, index+1, tour.length-index);
		new_tour[index] = city;
		
		//recompute distance travelled
		double new_distance_travelled = distance_travelled;
		int prev_index = index-1 >= 0? index-1 : new_tour.length-1;
		int next_index = index+1 < new_tour.length? index+1 : 0;
		new_distance_travelled += tsp.getDistance(new_tour[prev_index], city) 
				+ tsp.getDistance(city, new_tour[next_index]) 
				- tsp.getDistance(new_tour[prev_index], new_tour[next_index]);
		
		return new Tour(tsp, new_tour, new_distance_travelled);
	}
	
	/**
	 * @return The total distance travelled by the salesman in this tour
	 */
	public double getDistanceTravelled(){
		return distance_travelled;
	}
	
	/**
	 * @return the number of cities visited by the salesman in this tour
	 */
	public int getCitiesVisited(){
		return tour.length;
	}
	
	public String toString(){
		String res = "";
		if(tour.length > 0){
			for(int i = 0;i < tour.length-1;i++){
				res += tour[i]+" -> "+tour[i+1]+ " [distance: " + tsp.getDistance(tour[i], tour[i+1]) + "]" +System.lineSeparator();
			}
			res += tour[tour.length-1]+" -> "+tour[0]+ " [distance: " + tsp.getDistance(tour[tour.length-1], tour[0]) + "]" + System.lineSeparator();
		}else{
			res += "<empty tour>"+ System.lineSeparator();
		}
		res += "# unvisited cities: "+(tsp.getNumberOfCities()-tour.length) + System.lineSeparator();
		res += "total distance travelled: "+distance_travelled + System.lineSeparator();
		return res;
	}
	

}
