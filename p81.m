matrix = [131	673	234	103	18;
201	96	342	965	150;
630	803	746	422	111;
537	699	497	121	956;
805	732	524	37	331]

load matrix.txt

size = 80;
for i = 2:size
	for j = 1:i
		xc = j;
		yc = i+1-j;
		if (j > 1)
			o1 = matrix(xc - 1,yc);
		else
			o1 = 999999
		end
		if (j < i)
			o2 = matrix(xc, yc-1);
		else
			o2 = 999999
		end
		matrix(xc,yc) += min(o1,o2);
	end
end
for i = (size-1):-1:1
	yc = size;
	for j = (size + 1 - i):size
		xc = j;
		matrix(xc,yc) += min(matrix(xc-1,yc),matrix(xc,yc-1));
		yc -= 1;
	end
end
