%define name    jazzhands-hotpants
%define release 0
Name:   	jazzhands-hotpants
Version:        __VERSION__
Release:        0%{?dist}
Summary:        JazzHands HOTPants clients

Group:  	System Environment/Libraries
License:        BSD
URL:    	http://www.jazzhands.net/
Source0:        %{name}-%{version}.tar.gz
BuildRoot:      %{_tmppath}/%{name}-%{version}-%{release}-buildroot
BuildArch:	noarch
%if 0%{?suse_version}
%else
%if 0%{?rhel} < 6
BuildRequires: perl(ExtUtils::MakeMaker)
%else
BuildRequires: perl-ExtUtils-MakeMaker
%endif
%endif
Requires:	jazzhands-perl-hotpants = %{version}

%description

JazzHands HOTPants clients

%package -n jazzhands-perl-hotpants
group: System Environment/Libraries
Summary: JazzHands HOTPants perl libraries
Requires: perl-Net-IP, perl-Net-DNS, perl-NetAddr-IP, jazzhands-perl-common >= 0.69.0, perl-Crypt-Rijndael, perl-Crypt-Eksblowfish, perl-Digest-HMAC, perl-Crypt-CBC, perl-DateTime-Format-Strptime, perl-URI

%package -n jazzhands-hotpants-perl-rlm
group: System Environment/Libraries
Summary: FreeRadius module for JazzHands HOTPants
Requires: jazzhands-perl-hotpants = %{version}


%description -n jazzhands-perl-hotpants
JazzHands HOTPants perl libraries

%description -n jazzhands-hotpants-perl-rlm
FreeRadius module for JazzHands HOTPants

%prep
%setup -q -n %{name}-%{version}
make -f Makefile.jazzhands BUILDPERL=%{__perl}

%install
make -f Makefile.jazzhands INSTALLROOT=%{buildroot} prefix=%{prefix} BUILDPERL=%{__perl} install

%clean
make -f Makefile.jazzhands clean

%files -f debian/jazzhands-hotpants.install
%defattr(755,root,root,-)

# diverged enough between older redhat/suse and debian...
# -f debian/jazzhands-perl-hotpants.install
%files -n jazzhands-perl-hotpants 
%{perl_vendorlib}/*
# %{_mandir}/man3/*

%files -n jazzhands-hotpants-perl-rlm -f debian/jazzhands-hotpants-perl-rlm.install
