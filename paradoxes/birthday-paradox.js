// P-1.35 The birthday paradox says that the probability that two people in a room
// will have the same birthday is more than half, provided n, the number of
// people in the room, is more than 23. This property is not really a paradox,
// but many people ﬁnd it surprising. Design a Python program that can test
// this paradox by a series of experiments on randomly generated birthdays,
// which test this paradox for n = 5, 10, 15, 20, . . . , 100.

// Simulation approach
const birthDayProb = (N, hits=0, reps=10**4) => {
    for(let i = 0; i < reps; i++){
        const birthDates = Array.from({length: N}, () => Math.floor(Math.random() * 365))
        const uniqueBirthDates = new Set(birthDates)
        if (uniqueBirthDates.size < birthDates.length) hits += 1        
    }
    return hits / reps
}

// Closed-form approach. Exploits complementation.
const birthDayClosedForm = (N, days=365) => {
    prod = 1
    for(let i = 0; i < N; i++) prod *= (days - i)
    return 1 - prod / (days**N)
}

// Calculate probability, first by simulation, then in closed form. Compare.
const pop = Array.from(new Array(20), (x, i) => 5 * i + 5) // array by fives, corrected for zero index
simProbs = pop.map(x => birthDayProb(x))
closedProbs = pop.map(x => birthDayClosedForm(x))

// How many reps do we need to have an arbitrarily low error?
meanAbsDev = (v1, v2) => {
    totalError = 0
    for(let i = 0; i < v1.length; i++) totalError += Math.abs(v1[i] - v2[i]) 
    return totalError / v1.length
} 

// Print mean absolute deviation over population vector
console.log("Mean absolute deviation over entire population vector:", (100 * meanAbsDev(simProbs, closedProbs)).toFixed(3), "%")

// Example: contrast the simulated value
percentile50Sim = birthDayProb(23)
percentile50Closed = birthDayClosedForm(23)
console.log("The probability that at least 2 of 23 randomly selected people have the same birthday, by simulation, is", (100 * percentile50Sim).toFixed(3) + "%")
console.log("The probability that at least 2 of 23 randomly selected people have the same birthday, in closed-form, is", (100 * percentile50Closed).toFixed(3) + "%")
