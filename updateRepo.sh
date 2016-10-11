sha=$(git ls-remote https://github.com/theNerd247/blaze-bootstrap.git\
	| grep additions-nh \
	| cut -f1\
	)

pat='$!N; s/\(.*blaze-bootstrap.*\n.*commit:\).*$/\1 '$sha'/g'
sed -i.bak -e "$pat" stack.yaml
