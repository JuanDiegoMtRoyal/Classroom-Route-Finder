public class Stairs extends Node{
    public Stairs(String name, Hallway hallway, int positionAlongHallway, int floor)
    {
        super(name, hallway, positionAlongHallway, floor);
    }

    @Override
    public void displayInfo() {
        System.out.println("Stairs: " + name + " at " + positionAlongHallway + "m along " + hallway.name);
    
}
