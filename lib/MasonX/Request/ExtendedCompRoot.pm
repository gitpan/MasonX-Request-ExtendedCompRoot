# This software is copyright (c) 2004 Alex Robinson.
# It is free software and can be used under the same terms as perl,
# i.e. either the GNU Public Licence or the Artistic License.

package MasonX::Request::ExtendedCompRoot;

use strict;

our $VERSION = '0.03';

use base qw(HTML::Mason::Request);

# fetch_comp needs this
use HTML::Mason::Tools qw(absolute_comp_path);
use HTML::Mason::Exceptions( abbr => [qw(param_error error)] ); 
use Params::Validate qw(:all);
Params::Validate::validation_options( on_fail => sub { param_error( join '', @_ ) } );
use File::Spec;# qw(canonpath file_name_is_absolute);

# this absurdity, because there's a bug if you leave it out or pass no args
__PACKAGE__->valid_params(blop => 0);

#
# Standard request subclass alter_superclass dance
#
sub new
{
    my $class = shift;
    $class->alter_superclass( $HTML::Mason::ApacheHandler::VERSION ?
                              'HTML::Mason::Request::ApacheHandler' :
                              $HTML::Mason::CGIHandler::VERSION ?
                              'HTML::Mason::Request::CGI' :
                              'HTML::Mason::Request' );
    return $class->SUPER::new(@_);
}

#
# Given a component path (absolute or relative), returns a component.
# Handles SELF, PARENT, REQUEST, comp:method, relative->absolute
# conversion, and local subcomponents.
#
# Basically copied and pasted from HTML::Mason::Request
#Êsee inline comments for diffs
#
sub fetch_comp
{
    my ($self,$path) = @_;
    param_error "fetch_comp: requires path as first argument" unless defined($path);

    #
    # Handle paths SELF and PARENT
    #
    if ($path eq 'SELF') {
	return $self->base_comp;
    }
    if ($path eq 'PARENT') {
	my $c = $self->current_comp->parent
	    or error "PARENT designator used from component with no parent";
	return $c;
    }
    if ($path eq 'REQUEST') {
        return $self->request_comp;
    }

##### Additions/Changes to Request.pm #####
	#
	# Handle paths of the form comp_root=>comp_path
	# Make calls to specific comp_roots appear to be absolute paths
	if (index($path,'=>') != -1)
		{
		$path =~ s|^/*|/|;
		}

    #
    # Handle paths of the form comp_path:method_name
    #
    if (index($path,':') != -1) {
	my ($method_comp,$owner_comp);
	my ($owner_path,$method_name) = split(':',$path,2);
	
	my @comp_root = $self->comp_root;
	foreach (@comp_root)
		{
		$owner_comp = undef;
		my $owner_path_with_comp_root = $owner_path;
		if ($owner_path_with_comp_root =~ m/^(SELF|PARENT|REQUEST)$/)
			{
			# leave alone - these are the special paths
			}
		elsif (index($owner_path_with_comp_root,'=>') == -1)
			{
			# make comp_path into comp_root=>comp_path unless it already is
			$owner_path_with_comp_root = $_->[0].'=>'.$owner_path_with_comp_root;
			}
		$owner_comp = $self->fetch_comp($owner_path_with_comp_root);
		next unless ($owner_comp);
		$owner_comp->_locate_inherited('methods',$method_name,\$method_comp);
		# nothing more to be done since, success, we have a method_comp
		last if ($method_comp);
		# nothing more to be done as path is special or an explicit comp_root call
		last if $owner_path eq $owner_path_with_comp_root;
		}
# now do the error handling
	unless ($owner_comp) { error "could not find component for path '$owner_path'\n"; }
	unless ($method_comp) { error "no method '$method_name' for component " . $owner_comp->name; }
# owner_comp->title tweaked to owner_comp->name
	return $method_comp;
    }
##### Additions to Request.pm end #####

    #
    # If path does not contain a slash, check for a subcomponent in the
    # current component first.
    #
    if ($path !~ /\//) {
	my $cur_comp = $self->current_comp;
	# Check my subcomponents.
	if (my $subcomp = $cur_comp->subcomps($path)) {
	    return $subcomp;
	}
	# If I am a subcomponent, also check my owner's subcomponents.
	# This won't work when we go to multiply embedded subcomponents...
	if ($cur_comp->is_subcomp and my $subcomp = $cur_comp->owner->subcomps($path)) {
	    return $subcomp;
	}
    }

    #
    # Otherwise pass the absolute path to interp->load.
    #
    # For speed, don't call ->current_comp, instead access it directly
    $path = absolute_comp_path($path, $self->{stack}[-1]{comp}->dir_path)
	unless substr($path, 0, 1) eq '/';

    my $comp = $self->interp->load($path);

    return $comp;
}

#
# Call Request.pm's exec, then put comp_root back 
# to what it was when the current request or subrequest was made
#
sub exec
	{
	my $self = shift;
	my $return_exec = $self->SUPER::exec(@_);
	$self->reset_comp_root;
	return $return_exec;
	}

#
# return comp_root array and optionally set it
#
sub comp_root
	{
	my $self = shift;
	my @comp_root = $self->_validate_comp_root_args(@_);
	if (@comp_root)
		{
		$self->_store_comp_root;
		$self->interp->resolver->{comp_root} = \@comp_root;
		}
	return @{$self->interp->resolver->{comp_root}};
	}

#
# add further comp_roots to the beginning of the comp_root array
#
sub prefix_comp_root
	{
	my $self = shift;
	my @prefix_comp_root = $self->_validate_comp_root_args(@_);
	return unless (@prefix_comp_root);
	my %seen;
	foreach my $root (@{$self->interp->resolver->{comp_root}})
		{
		$seen{$root->[0]} = 1;
		}
	foreach my $root (@prefix_comp_root)
		{
	   	param_error "comp_root '$root->[0]' already exists" if ($seen{$root->[0]});
    	}
	$self->_store_comp_root;
	unshift(@{$self->interp->resolver->{comp_root}}, @prefix_comp_root);
	return;
	}

#
# put comp_root back to how it was at the beginning of the current (sub)request
#
sub reset_comp_root
	{
	my $self = shift;
	if ($self->{store_comp_root})
		{
		my @copy_store = @{$self->{store_comp_root}};
		$self->interp->resolver->{comp_root} = \@copy_store;
		$self->{store_comp_root} = undef;
		}
	return;
	}

#
# make sure we know what the original comp_root was
#
sub _store_comp_root
	{
	my $self = shift;
	unless ($self->{store_comp_root})
		{
		my @copy_root = @{$self->interp->resolver->{comp_root}};
		$self->{store_comp_root} = \@copy_root;
		}
	return;
	}

#
# make sure that args are valid and marshall into required array format
#
sub _validate_comp_root_args
	{
	my $self = shift;
	my @check_comp_root = @_;
	
	my $array_check = ref($check_comp_root[0]);
	if ($array_check eq "ARRAY")
		{
		my $inner_array_check = ref($check_comp_root[0][0]);
		if  ( ($inner_array_check =~ /ARRAY|HASH/s) or (index($check_comp_root[0][0],'=>') != -1) )
			{
			for (my $i = scalar(@{$check_comp_root[0]}) - 1; $i >= 0; $i--)
				{
				$check_comp_root[$i] = $check_comp_root[0][$i];
				}
			}
		}
	
	my %seen;
	foreach (@check_comp_root)
		{
		my $ref_check = ref($_);
		unless ($ref_check =~ m/ARRAY|HASH/)
			{
			my $string = $_;
			if (index($string,'=>') != -1)
				{
				my @root_split = split('=>', $string);
				if ($root_split[2]) { die "Too many delimiters in comp_root $string"; }
				$_ = \@root_split;
				}
			else { die "$string is wrong - comp_root should take the form 'comp_root_name=>/path/to/comp_root'"; }
			}
		if ($ref_check =~ m/HASH/)
			{
			my %hash_store = %{$_};
			foreach my $key (keys %hash_store)
				{
				$_ = [$key, $hash_store{$key}];
				}
			}
		$_->[1] = File::Spec->canonpath( $_->[1] );
		param_error "path specified for comp_root '$_->[0]' ($_->[1]) is not an absolute directory" unless File::Spec->file_name_is_absolute( $_->[1] );
		param_error "path specified for comp_root '$_->[0]' ($_->[1]) does not exist" unless (-d $_->[1]);
		param_error "comp_root '$_->[0]' should only be specified once" if $seen{$_->[0]}++;
		}
	return @check_comp_root;
	}

1;


__END__

=head1 NAME

MasonX::Request::ExtendedCompRoot - Extend functionality of Mason's component root

=head1 SYNOPSIS

In your F<httpd.conf> file:

  PerlSetVar  MasonRequestClass   MasonX::Request::ExtendedCompRoot
  PerlSetVar  MasonResolverClass  MasonX::Resolver::ExtendedCompRoot

Or when creating an ApacheHandler object:

  my $ah =
      HTML::Mason::ApacheHandler->new
          ( request_class  => 'MasonX::Request::ExtendedCompRoot',
            resolver_class => 'MasonX::Resolver::ExtendedCompRoot'
            ...
          );

Once Mason is up and running, ExtendedCompRoot allows you to:

  # completely override the component root
  $m->comp_root({key1=>'/path/to/root1'}, {key2=>'/path/to/root2'});
  
  # add another root to the component root
  $m->prefix_comp_root('key=>/path/to/root');
  
  # call a component in a specific component root
  <& key=>/path/to/comp &>

C<MasonX::Request::ExtendedCompRoot> can also be used as the request class when running Mason in standalone mode.

=head1 DESCRIPTION

=head2 DYNAMIC COMPONENT ROOT

C<MasonX::Request::ExtendedCompRoot> lets you alter Mason's component root during the lifetime of any given request or subrequest.

This behaviour is useful if you want to override certain components, but cannot determine that at the moment you create your handler (when you could in theory create an interp object with a different component root) or because you configure Mason in an F<httpd.conf>.

For example:

  # outputs component in /path/to/root1
  <& /path/to/comp &>
  
  % $m->prefix_comp_root('another_key=>/path/to/root2');
  
  # now outputs component in /path/to/root2 if it exists
  # if it doesn't, the output remains the component in /path/to/root1
  <& /path/to/comp &>

At the end of each request or subrequest, the component root is reset to its initial state.

=head2 ADDITIONAL COMPONENT CALL SYNTAX

C<MasonX::Request::ExtendedCompRoot> also provides syntactical glue to enable calling a component in a specific component root that would otherwise be inaccessible via the usual search path.

  <& key=>/path/to/comp &>

ie. A given component path matches the first file found in an ordered search through the roots, but if preceded with the named key of a component root, matches the file found in that root.

This leaves the rules for calling methods that deal with component paths (C<$m->comp>, C<$m->comp_exists>, C<$m->fetch_comp>) as follows:

=over 4

=item * If the path is absolute (starting with a '/'), then the component is found relative to the component root.

=item * If the path contains the component root delimiter ('=>'), then the component is found in the specified component root.

=item * If the path is relative (no leading '/'), then the component is found relative to the current component directory.

=item * If the path matches both a subcomponent and file-based component, the subcomponent takes precedence.

=head2 QUASI-INHERITANCE FOR MASON METHODS

Calls to component methods work slightly differently to standard Mason behaviour.

The standard behaviour is:

  a) search component root for component
  b) if a component is found, look up method in that component
  c) stop regardless of whether a method exists there or not

This makes perfect sense since a given component path can only ever match the first file found in an ordered search through the roots. However, that doesn't hold true for C<MasonX::Request::ExtendedCompRoot>.

The ExtendedCompRoot way is:

  a) search component root for component
  b) if a component is found, look up method in that component
  c) if no method is found in that component, continue searching the component root

This has the effect of allowing methods to percolate up through the component root as if they were "inherited".

For example, given a component root [key1=>'path/to/root1', key2=>'/path/to/root2'], if '/path/to/comp' exists in both roots, but only the component in key2 has a method called 'method_name':

  # outputs component in key1
  <& /path/to/comp &> 

  # outputs method in key2
  <& /path/to/comp:method_name &>

NB. in the above example, only the method is accessed - the components in key1 and key2 are separate and variables are not shared between them. If you wanted such behaviour, you would either need to make any such variables global or else use $m->notes or even create get and set methods to pass the variables back and forth.

=head1 USAGE

=head2 SET UP

To use this module you need to tell Mason to use this class for requests and C<MasonX::Resolver::ExtendedCompRoot> for its resolver.  This can be done in two ways.  If you are configuring Mason via your F<httpd.conf> file, simply add this:

  PerlSetVar  MasonRequestClass    MasonX::Request::ExtendedCompRoot
  PerlSetVar  MasonResolverClass   MasonX::Resolver::ExtendedCompRoot

If you are using a F<handler.pl> file, simply add this parameter to
the parameters given to the ApacheHandler constructor:

  request_class  => 'MasonX::Request::ExtendedCompRoot'
  resolver_class => 'MasonX::Resolver::ExtendedCompRoot'

=head2 METHODS

=over 4

=item * comp_root

Returns an array of component roots if no arguments are passed.

  my @comp_root = $m->comp_root;     # just returns the comp_root

If any arguments are passed, the existing component root is replaced with those values.

Any argument passed must have a name and a path. The name must be unique and the path absolute and actually exist.

Arguments can be passed as an argument list of scalars, hashes or arrays (or a combination of any of those) or as an array ref to such a list - the following examples are all equivalent:
  
  # as scalar - must take form NAME=>PATH
  $m->comp_root('key1=>/path/to/root1', 'key2=>/path/to/root2');

  $m->comp_root({key1=>'/path/to/root1'}, {key2=>'/path/to/root2'});

  $m->comp_root(['key1','/path/to/root1'], ['key2','/path/to/root2']);

  $m->comp_root('key1=>/path/to/root1', {key2=>'/path/to/root2'}, ['key3','/path/to/root3']);

  my @new_comp_root = ('key1=>/path/to/root1', 'key2=>/path/to/root2');
  $m->comp_root(\@new_comp_root);

=item * prefix_comp_root

Adds passed arguments to the beginning of the current component root array.

  $m->prefix_comp_root('key=>/path/to/root');

Arguments for C<prefix_comp_root> are treated in exactly the same way as those for C<comp_root>.

=item * reset_comp_root

Resets the component root to how it was at the start of the current request or subrequest. Takes no arguments.

NB. At the end of each request or subrequest, reset_comp_root is called to ensure that the component root returns to its initial state.

=back

=head1 PREREQUISITES

HTML::Mason

=head1 BUGS

No known bugs. Unless the inability to insert non-existent or non-absolute paths into the component root is considered a bug.

=head1 VERSION

0.03

=head1 SEE ALSO

L<HTML::Mason>, L<MasonX::Resolver::ExtendedCompRoot>, L<MasonX::Request::WithApacheSession>

=head1 AUTHOR

Alex Robinson, <cpan[@]alex.cloudband.com>

=head1 LICENSE

MasonX::Request::ExtendedCompRoot is free software and can be used under the same terms as Perl, i.e. either the GNU Public Licence or the Artistic License.

=cut
