.salaries as $salaries | .staff[] | "\(.name) makes $\($salaries[.id])
