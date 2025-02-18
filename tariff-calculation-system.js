/**
 * 📦 **Optimized Tariff Calculation System**
 * ===========================================
 * This program calculates import tariffs based on:
 * - **Arrival Country** (Destination country)
 * - **Origin Country** (Where the product is shipped from)
 * - **Product Category** (e.g., electronics, clothing, furniture)
 * - **Material Type** (e.g., silk, cotton, wool)
 * 
 * 🚀 **How It Works:**
 * 1️⃣ **Tariff Rules Lookup Object**:
 *    - Instead of millions of `if` statements, we use a **nested object** (`tariffRules`).
 *    - This allows for **fast lookups (O(1) complexity)** without unnecessary condition checks.
 *    
 * 2️⃣ **Tariff Rate Retrieval**:
 *    - Uses `getTariffRate(arrival, origin, category, material)`.
 *    - Checks tariff rates based on **country, category, and material**.
 *    - Returns **0% tariff if no matching rule is found**.
 *    
 * 3️⃣ **Final Price Calculation**:
 *    - The `applyTariff(product, origin, arrival, materials)` function:
 *      - Retrieves the **tariff rate**.
 *      - Computes the **final price** using:  
 *        `finalPrice = originalPrice * (1 + tariffRate)`.
 * 
 * 🎯 **Why This Approach?**
 * - ✅ **Fast & Efficient**: No need to check thousands of conditions manually.
 * - ✅ **Scalable**: Easily add more rules without changing logic.
 * - ✅ **Flexible**: Can support new product categories, materials, or tariff exceptions.
 * 
 * 🛠️ **Example Usage:**
 * ```javascript
 * const product = { category: "clothing", originalPrice: 100.00 };
 * const result = applyTariff(product, "Vietnam", "USA", ["silk"]);
 * console.log(result.tariffRate, result.finalPrice);
 * ```
 * 
 * 🔥 **Future Improvements:**
 * - Connect with **real-world tariff databases** (e.g., WTO, U.S. Customs API).
 * - Support **multiple materials** (e.g., silk-cotton blends).
 * - Factor in **product weight, trade agreements, and price tiers**.
 * 
 * Author: Teg Louis
 * Created: 2/17/2025
 */


// Define tariff rates in a structured lookup object
const tariffRules = {
    "USA": { 
        "China": { 
            "electronics": 0.25 
        },
        "Vietnam": {
            "clothing": {
                "silk": 0.20,
                "cotton": 0.10
            }
        }
    },
    "EU": {
        "China": { 
            "electronics": 0.15 
        },
        "Vietnam": {
            "clothing": {
                "wool": 0.12
            }
        }
    }
};

// Optimized function to get the tariff rate
function getTariffRate(arrival, origin, category, material) {
    return tariffRules?.[arrival]?.[origin]?.[category]?.[material] ||
           tariffRules?.[arrival]?.[origin]?.[category] ||
           0; // Default: No tariff
}

// Apply tariff and calculate final price
function applyTariff(product, origin, arrival, materials) {
    let tariffRate = getTariffRate(arrival, origin, product.category, materials[0]); // First material match

    product.finalPrice = product.originalPrice * (1 + tariffRate);

    return { tariffRate, finalPrice: product.finalPrice };
}

// Example Product
const product = {
    category: "clothing",
    originalPrice: 100.00
};

const result = applyTariff(product, "Vietnam", "USA", ["silk"]);
console.log(`Tariff Rate: ${result.tariffRate}`);
console.log(`Final Price After Tariff: $${result.finalPrice.toFixed(2)}`);
