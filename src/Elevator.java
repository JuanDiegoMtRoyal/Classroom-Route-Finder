public class Elevator extends Node{
    
    public Elevator(String name, Hallway hallway, int positionAlongHallway, int floor) {
        super(name, hallway, positionAlongHallway, floor);
    }

    @Override
    public void displayInfo() {
        System.out.println("Elevator: " + name + " at " + positionAlongHallway + "m along " + hallway.name);
    }
}
