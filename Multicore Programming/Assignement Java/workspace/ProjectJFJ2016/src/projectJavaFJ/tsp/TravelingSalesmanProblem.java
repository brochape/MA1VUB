package projectJavaFJ.tsp;
import java.util.Random;

/**
 * This class is used to represent instances of the Traveling Salesman Problem.
 * Given a set of cities and their locations, find the shortest possible route 
 * that visits each city exactly once and returns to the origin city.
 * Locations are represented as 2D integer coordinates and the Euclidean distance is used as distance measure.
 *
 * @author Steven Adriaensen
 *
 */
public class TravelingSalesmanProblem {
	final private Random rng;
	final private Location[] cities;
	final private int n;

	/**
	 * Constructs a random tsp instance with n cities.
	 * Locations of cities are drawn uniformly at random with 0 <= x,y < 1000
	 * using a random number generator using the current time as seed.
	 * @param n: The number of cities to visit.
	 */
	public TravelingSalesmanProblem(int n){
		this(n,new Random());
	}
	
	/**
	 * Constructs a random tsp instance with n cities.
	 * Locations of cities are drawn uniformly at random with 0 <= x,y < 1000 
	 * using a given random number generator.
	 * @param n: The number of cities to visit.
	 * @param rng: The random number generator used.
	 * 
	 */
	public TravelingSalesmanProblem(int n, Random rng){
		this.n = n;
		this.rng = rng;
		cities = new Location[n];
		for(int i = 0; i < n;i++){
			cities[i] = new Location();
		}
	}
	
	/**
	 * @return The number of cities to visit.
	 */
	public int getNumberOfCities(){
		return n;
	}
	
	/**
	 * Constructs an empty solution, visiting 0 cities.
	 * @return An empty solution
	 */
	public Tour getEmptySolution(){
		return new Tour(this);
	}
	
	/**
	 * Computes the Euclidean distance between 2 cities. <br>
	 * Cities are referenced using id (cid). <br>
	 * @param city1 id of city of origin
	 * @param city2 id of destination city
	 * @return The Euclidean distance between the location of city 1 and city 2
	 */
	public double getDistance(int city1, int city2){
		int delta_x = cities[city1].x-cities[city2].x;
		int delta_y = cities[city1].y-cities[city2].y;
		return Math.sqrt(delta_x*delta_x+delta_y*delta_y);
	}
	
	/**
	 * This class represents a coordinate in I x I (2D integer space)
	 * 
	 * @author Steven Adriaensen
	 *
	 */
	class Location{
		int x = rng.nextInt(1000);
		int y = rng.nextInt(1000);
	}
	
	public String toString(){
		return "TravelingSalesmanProblem(n: "+n+")";
	}

}
