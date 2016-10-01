package projectJavaFJ.tsp.test;

import projectJavaFJ.tsp.TravelingSalesmanProblem;

import java.util.Random;

import projectJavaFJ.tsp.Tour;

/**
 * A wrapper class for VehicleRoutingProblem, counting the number of insertions performed by a solver
 * THIS CLASS SHOULD ONLY BE USED FOR TESTING PURPOSES (use VehicleRoutingProblem in your experiments!)  
 * 
 * @author Steven Adriaensen
 *
 */
public class TravelingSalesmanProblemTest extends TravelingSalesmanProblem{
	
	public TravelingSalesmanProblemTest(int n, Random rng) {
		super(n, rng);
	}
	
	public TravelingSalesmanProblemTest(int n) {
		super(n);
	}

	@Override
	public Tour getEmptySolution(){
		return new TourTest(this);
	}

}
