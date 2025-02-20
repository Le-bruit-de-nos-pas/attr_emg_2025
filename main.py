import json

# Load Coefficients and Variables from JSON files
def load_data(coefficients_file, variables_file):
    with open(coefficients_file, 'r') as file:
        coefficients = json.load(file)

    with open(variables_file, 'r') as file:
        variables = json.load(file)

    return coefficients, variables




# Function to calculate the score
def calculate_score(model_name, user_inputs, coefficients, variables):
    model_vars = variables[model_name]
    model_coefs = coefficients[model_name]
    
    # Multiply coefficients by corresponding user inputs and sum them
    score = model_coefs[0]  # Start with the intercept term
    for i in range(1, len(model_vars)):  # Start from index 1 to skip intercept
        score += user_inputs[model_vars[i]] * model_coefs[i]

    return score

# Main function to prompt the user for input and calculate the score
def main():
    # Load data from JSON files
    coefficients, variables = load_data('model_coefficients.json', 'model_variables.json')
    
    # Ask user to select a model
    print("Available models: ", list(variables.keys()))
    model_name = input("Please select a model (e.g., Model_1): ")
    
    # Check if model is valid
    if model_name not in variables:
        print(f"Invalid model: {model_name}")
        return
    
    # Prompt user for input values for selected model
    model_vars = variables[model_name]
    user_inputs = {}

    for var in model_vars:
        if var != "(Intercept)":  # Skip intercept variable
            value = input(f"Please enter the value for {var}: ")
            user_inputs[var] = float(value)

    # Calculate and display the result
    score = calculate_score(model_name, user_inputs, coefficients, variables)
    print(f"Total Score for {model_name}: {score}")

# Run the program
if __name__ == "__main__":
    main()
