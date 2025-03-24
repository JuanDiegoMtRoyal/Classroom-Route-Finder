public class Classroom extends Node{

        String compassDirection;
    
        public Classroom(String name, String building, Hallway hallway, int positionAlongHallway, String compassDirection, int floor) {
            super(name, hallway, positionAlongHallway, floor);
            this.compassDirection = compassDirection;
        }
    
        @Override
        public void displayInfo() {
            System.out.println("Go " + compassDirection + " towards Classroom: " + name + " at " + positionAlongHallway + "m along " + hallway.name);
        }
    
}
