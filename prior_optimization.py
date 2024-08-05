import numpy as np
from scipy.optimize import minimize, differential_evolution

# Define the objective function
def objective(L_flat, A, m, reg=1e-3, threshold=0.001):
    # Reshape L_flat back to a matrix form (not necessarily lower triangular)
    L = L_flat.reshape((m, m))
    
    # Compute P = LL^T (ensures P is symmetric and positive semi-definite)
    P = L @ L.T
    
    # Compute (I_m - PP^T)
    I_m = np.eye(m)
    PPt = P @ P.T
    I_m_minus_PPt = I_m - PPt
    
    # Add a larger regularization term to ensure positive definiteness
    I_m_minus_PPt += reg * np.eye(m)
    
    # Check if the matrix is positive definite
    eigenvalues = np.linalg.eigvals(I_m_minus_PPt)
    if np.any(eigenvalues <= 0):
        return np.inf
    
    # Compute (I_m - PP^T)^(-1/2) using eigendecomposition
    eigenvalues, eigenvectors = np.linalg.eigh(I_m_minus_PPt)
    eigenvalues_inv_sqrt = np.diag(1.0 / np.sqrt(eigenvalues))
    I_m_minus_PPt_inv_sqrt = eigenvectors @ eigenvalues_inv_sqrt @ eigenvectors.T
    
    # Compute the RHS of the equation
    RHS = I_m_minus_PPt_inv_sqrt @ P
    
    # Compute the difference between LHS (A) and RHS
    diff = A - RHS
    norm_diff = np.linalg.norm(diff)
    
    # Add a penalty if the norm of the difference is greater than the threshold
    if norm_diff > threshold:
        penalty = 1e6 * (norm_diff - threshold) ** 2
    else:
        penalty = 0
    
    # Return the norm of the difference as the objective value with penalty
    return norm_diff + penalty

# Example usage
def find_P_from_A(A):
    m = A.shape[0]
    
    # Initialize L with an identity matrix scaled by a small value
    L_initial = np.eye(m) * 0.1
    bounds = [(0, 1) for _ in range(m * m)]
    
    # Perform optimization using Differential Evolution
    result = differential_evolution(objective, bounds, args=(A, m), strategy='best1bin', maxiter=10000, tol=1e-12, disp=True)
    
    # Reshape the optimized L_flat back to matrix form
    L_optimized = result.x.reshape((m, m))
    
    # Compute P from the optimized L
    P_optimized = L_optimized @ L_optimized.T
    
    return P_optimized

# Example matrix A (replace with your actual matrix A)
A = np.array([[-0.2237266, 0.50376704], [-0.06878603, 1.03237671]])

# Find P from A using optimization
P_optimized = find_P_from_A(A)

# Compute the function of P
def compute_function_of_P(P, reg=1e-3):
    m = P.shape[0]
    I_m = np.eye(m)
    PPt = P @ P.T
    I_m_minus_PPt = I_m - PPt
    print("I_m_minus_PPt:")
    print(I_m_minus_PPt)
    
    # Ensure the matrix is positive definite
    I_m_minus_PPt += reg * np.eye(m)
    
    # Compute (I_m - PP^T)^(-1/2) using eigendecomposition
    eigenvalues, eigenvectors = np.linalg.eigh(I_m_minus_PPt)
    print("Eigenvalues in compute_function_of_P:", eigenvalues)
    if np.any(eigenvalues <= 0):
        return np.inf
    
    eigenvalues_inv_sqrt = np.diag(1.0 / np.sqrt(eigenvalues))
    I_m_minus_PPt_inv_sqrt = eigenvectors @ eigenvalues_inv_sqrt @ eigenvectors.T
    
    # Compute (I_m - PP^T)^(-1/2) P
    result = I_m_minus_PPt_inv_sqrt @ P
    
    return result

# Compute the function of P using the optimized P matrix
function_of_P = compute_function_of_P(P_optimized)

# Calculate the difference between A and the function of P
difference = A - function_of_P

# Compute the norm of the difference
error_norm = np.linalg.norm(difference)

# Output the results
print("Optimized P:", P_optimized)
print("Function of P:\n", function_of_P)
print("Difference (A - function of P):\n", difference)
print("Norm of the difference:", error_norm)