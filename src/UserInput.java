import java.util.List;
import java.util.Scanner;

public class UserInput {
    private CRF crf;
    private Scanner scanner;

    public UserInput(CRF crf) {
        this.crf = crf;
        this.scanner = new Scanner(System.in);
    }

    public void start() {
        System.out.println("Welcome to the Classroom Finder for MRU Bisset Building!");
        System.out.println("Type 'exit' at any time to quit.\n");

        while (true) {
            // Get Start Room
            String startRoom = getStringInput("Enter your starting classroom (e.g., EB1113): ");
            if (startRoom == null) break;

            // Get End Room
            String endRoom = getStringInput("Enter the destination classroom (e.g., EB1028): ");
            if (endRoom == null) break;

            // Get Time Constraint
            int timeConstraint = getValidIntInput("Enter the maximum time (in minutes): ");
            if (timeConstraint == -1) break;

            // Get Mobility Constraints
            boolean mobilityConstraints = getBooleanInput("Do you have mobility constraints? (true for elevators only, false for stairs allowed): ");
            
            // Find and display the route
            System.out.println("\nFinding route...");
            List<Node> route = crf.findRoute(startRoom, endRoom, timeConstraint, mobilityConstraints);
            if (route == null) {
                System.out.println("No route found or classroom does not exist.\n");
            } else {
                System.out.println("Route found:");
                crf.displayRoute(route);
            }

            System.out.println("------------------------------------------------------\n");
        }

        System.out.println("Have a nice day!");
        scanner.close();
    }

    private String getStringInput(String prompt) {
        System.out.print(prompt);
        String input = scanner.nextLine().trim();
        if (input.equalsIgnoreCase("exit"))
        	return null;
        else
        	return input;
    }

    private int getValidIntInput(String prompt) {
        while (true) {
            System.out.print(prompt);
            String input = scanner.nextLine().trim();
            if (input.equalsIgnoreCase("exit")) return -1;

            try {
                int value = Integer.parseInt(input);
                if (value > 0) return value;
                System.out.println("Time constraint must be at least 1 minute.");
            } catch (NumberFormatException e) {
                System.out.println("Invalid input. Please enter a number.");
            }
        }
    }

    private boolean getBooleanInput(String prompt) {
        System.out.print(prompt);
        return Boolean.parseBoolean(scanner.nextLine().trim());
    }
}
