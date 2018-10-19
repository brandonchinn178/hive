provider "aws" {
  region = "us-west-2"
}

resource "aws_instance" "ci_build" {
  ami = "ami-28e07e50" # RHEL 7.5
  instance_type = "t3.medium"
  key_name = "hive-ci"
  security_groups = [
    "${aws_security_group.allow_all.name}",
  ]

  tags {
    Name = "Hive CI Build"
  }
}

resource "aws_security_group" "allow_all" {
  name = "allow_all"
  ingress {
    from_port = 0
    to_port = 0
    protocol = "-1"
    cidr_blocks = ["0.0.0.0/0"]
  }
}

output "ip" {
  value = "${aws_instance.ci_build.public_ip}"
}
