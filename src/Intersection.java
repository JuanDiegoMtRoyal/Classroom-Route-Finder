public class Intersection extends Node{
    public Intersection(String name, Hallway hallway, int positionAlongHallway, int floor) {
        super(name, hallway, positionAlongHallway, floor);
    }

    @Override
    public void displayInfo() {
        System.out.println("Intersection: " + name + " at " + positionAlongHallway + "m along " + hallway.name);
    }   
    
}
