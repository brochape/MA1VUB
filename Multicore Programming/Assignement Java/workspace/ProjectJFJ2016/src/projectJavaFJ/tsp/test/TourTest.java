package projectJavaFJ.tsp.test;

import java.util.concurrent.atomic.AtomicLong;

import projectJavaFJ.tsp.Tour;

/**
 * A wrapper class for Tour, counting the number of insertions performed by a solver
 * THIS CLASS SHOULD ONLY BE USED FOR TESTING PURPOSES (use Tour in your experiments!)  
 * 
 * @author Steven Adriaensen
 *
 */
public class TourTest extends Tour{
	AtomicLong insertions;
	
	public TourTest(Tour sol, AtomicLong insertions){
		super(sol);
		this.insertions = insertions;
	}
	
	public TourTest(TravelingSalesmanProblemTest tsp) {
		super(tsp);
		insertions = new AtomicLong(0);
	}
	
	public long getNumberOfInsertions(){
		return insertions.get();
	}
	
	@Override
	public TourTest visit(int city, int pos){
		TourTest sol = new TourTest(super.visit(city,pos),insertions);
		insertions.incrementAndGet();
		return sol;
	}

}
