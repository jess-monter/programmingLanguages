#!/usr/bin/ruby

class Perrin

	def perrin(n)
		if n == 0
			return 3
		elsif n == 1
			return 0
		elsif n == 2
			return 2
		else
			return perrin(n-2) + perrin(n-3)
		end	
	end

	# Create a new Perrin and call
	testPerrin= Perrin.new
	p "Test Perrin for n = 10"
	p testPerrin.perrin(10)

end